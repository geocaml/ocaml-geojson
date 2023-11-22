let src_of_flow ?(buff = Cstruct.create 4096) flow () =
  let got = Eio.Flow.(single_read flow buff) in
  let t = Cstruct.sub buff 0 got in
  t

let dst_of_flow flow b =
  match b with
  | bs -> Eio.Flow.(copy (cstruct_source [ bs ]) flow)
  | exception End_of_file -> ()
