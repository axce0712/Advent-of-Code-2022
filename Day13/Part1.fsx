type Packet =
    | List of Packet list
    | Integer of int

let rec parsePacket chars =
    match chars with
    | '[' :: cs -> List (parseList cs)
and parseList chars =

    
            

let example = @"[1,1,3,1,1]"

List [ List [ Integer 1]; List [ Integer 2; Integer 3; Integer 3 ] ]