type 'a t = {
  push: 'a -> unit;
  close: unit -> unit;
}

let push self x = self.push x
let close self = self.close ()
