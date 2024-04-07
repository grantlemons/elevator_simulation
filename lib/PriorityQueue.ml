type priority = int
type 'a pqueue =
| Empty
| Node of priority * 'a * 'a pqueue * 'a pqueue
let empty = Empty

let rec insert queue prio element = match queue with
| Empty -> Node(prio, element, Empty, Empty)
| Node(p, e, left, right) ->
    if prio <= p
    then Node(prio, element, insert right p e, left)
    else Node(p, e, insert right prio element, left)
exception Queue_is_empty

let rec remove_min = function
| Empty -> Empty
| Node(_, _, left, Empty) -> left
| Node(_, _, Empty, right) -> right
| Node(_, _, (Node(lprio, left_element, _, _) as left),
                  (Node(rprio, right_element, _, _) as right)) ->
    if lprio <= rprio
    then Node(lprio, left_element, remove_min left, right)
    else Node(rprio, right_element, left, remove_min right)

let rec count = function
| Empty -> 0
| Node(_, _, left, right) -> 1 + count left + count right

let extract = function
| Empty -> raise Queue_is_empty
| Node(prio, element, _, _) as queue -> (prio, element, remove_min queue)
