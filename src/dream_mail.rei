/** Common */;

module Address: {
  type t;

  let parse: string => option(t);

  let to_string: t => string;
};

[@deriving yojson]
type email = {
  address: Address.t,
  subject: string,
  text: string,
};

/** Plugins */;

module type Queue = {
  type t;

  let connect: unit => Lwt.t(t);

  let send: (t, string) => Lwt.t(unit);

  let receive: t => Lwt.t(option(string));
};

type email_plugin = {send: email => Lwt.t(unit)};

type queue_plugin = {queue: (module Queue)};

type plugin =
  | EmailP(email_plugin)
  | QueueP(queue_plugin);

let plugins: ref(list(plugin));

/** Interface */;

let queue_worker: unit => Lwt.t(unit);

let queue_email: email => Lwt.t(unit);
