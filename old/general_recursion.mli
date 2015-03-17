val expand : ('s, 't) General.fn -> ('s, 't, 'x) General.t -> ('s, 't, 'x) General.t
val already : ('s, 't, 'x) General.t -> 'x Option.t
val engine : ('s, 't) General.fn -> int -> ('s, 't, 'x) General.t -> ('s, 't, 'x) General.t
val petrol : ('s, 't) General.fn -> int -> 's -> 't Option.t
