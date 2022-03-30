module Ezjsonm = struct
  include Ezjsonm
  let find_opt t path =
    try Some (find t path) with Not_found -> None
end;;