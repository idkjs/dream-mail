open Lwt.Syntax;

/** Common */;

let (>>=) = Lwt.bind;

module Address = {
  [@deriving yojson]
  type t = string;

  let parse = address =>
    switch (Emile.of_string(address)) {
    | Ok(_) => Some(address)
    | Error(_) => None
    };

  let to_string = address => address;
};

[@deriving yojson]
type email = {
  address: Address.t,
  subject: string,
  text: string,
};

module type Queue = {
  type t;

  let connect: unit => Lwt.t(t);

  let send: (t, string) => Lwt.t(unit);

  let receive: t => Lwt.t(option(string));
};

/** Plugins */;

type email_plugin = {send: email => Lwt.t(unit)};

type queue_plugin = {queue: (module Queue)};

type plugin =
  | EmailP(email_plugin)
  | QueueP(queue_plugin);

let plugins = ref([]);

let queue_plugin = () =>
  List.find_map(
    fun
    | EmailP(_) => None
    | QueueP(queue) => Some(queue),
    plugins^,
  )
  |> Option.get;

let email_plugin = () =>
  List.find_map(
    fun
    | EmailP(email) => Some(email)
    | QueueP(_) => None,
    plugins^,
  )
  |> Option.get;

/** Interface */;

let handle_message = message => {
  let email =
    try(Some(message |> Yojson.Safe.from_string |> email_of_yojson)) {
    | _ =>
      Printf.printf(
        "Received invalid email record from RabbitMQ: %s",
        message,
      );
      None;
    };

  switch (email) {
  | Some(e) => email_plugin().send(e)
  | None => Lwt.return_unit
  };
};

let queue_worker = () => {
  Printf.printf("Starting Queue Worker\n");
  module Q = (val queue_plugin().queue: Queue);
  let* queue = Q.connect();
  let rec handle = () =>
    Q.receive(queue)
    >>= (
      message => {
        let task =
          switch (message) {
          | Some(m) =>
            flush(stdout);
            handle_message(m);
          | _ =>
            Printf.printf("No new tasks, waiting.\n");
            flush(stdout);
            Lwt_unix.sleep(5.);
          };

        task >>= handle;
      }
    );

  handle();
};

let queue_email = email => {
  module Q = (val queue_plugin().queue: Queue);
  let text = email |> yojson_of_email |> Yojson.Safe.to_string;
  let* queue = Q.connect();
  let* _ = Q.send(queue, text);
  Lwt.return_unit;
};

/** Main */;

let () = {
  let send = email => {
    Printf.printf(
      "Sending message with subject \"%s\" and message \"%s\" to address \"%s\"",
      email.subject,
      email.text,
      Address.to_string(email.address),
    );
    Lwt.return_unit;
  };

  plugins := [EmailP({send: send}), ...plugins^];

  module Q: Queue = {
    type t = Stdlib.Queue.t(string);

    let _queue = Stdlib.Queue.create();

    let connect = () => Lwt.return(_queue);

    let send = (queue, message) => {
      Stdlib.Queue.add(message, queue);
      Lwt.return_unit;
    };

    let receive = queue => Lwt.return @@ Stdlib.Queue.take_opt(queue);
  };
  plugins := [QueueP({queue: (module Q)}), ...plugins^];
  let email = {
    address: Option.get @@ Address.parse("test@test.com"),
    text: "Test body",
    subject: "Test subject",
  };

  Lwt_main.run(queue_email(email) >>= (_ => queue_worker()));
};
