(* Records test *)

type person = {
  name: string;
  age: int;
}

(* Record creation *)
let create_person name age = { name; age }

(* Field access *)
let get_name p = p.name
let get_age p = p.age

(* Record update *)
let birthday p = { p with age = p.age + 1 }

(* Multiple field update *)
let rename p new_name = { p with name = new_name }

(* Nested records *)
type address = {
  street: string;
  city: string;
}

type employee = {
  person: person;
  address: address;
}

let create_employee name age street city =
  { person = { name; age }; address = { street; city } }

let get_employee_name e = e.person.name
