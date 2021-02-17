# *Juniper*
### A  macro-based swagger binding generator for Common Lisp

Juniper provides a couple of macros that generates code for interacting with web APIs based on a Swagger/OpenAPI schema; it is designed to allow Common Lisp to seamlessly interact with web services over HTTP just as if they were local Lisp packages. Currently, only Swagger 2.0 schemas are supported.

## Installation

Clone the repository to somewhere ASDF can find, then
```lisp
(asdf:load-system :juniper)
```

Juniper is not yet installable with quicklisp.

## Examples

The following example generates bindings for the "https://petstore.swagger.io/v2/swagger.json" schema and makes a couple of requests to the remote server:

```lisp
> (juniper:bindings-from-url "https://petstore.swagger.io/v2/swagger.json")
CREATE-USER

> (get-inventory)
((:ARTHUR . 1) (:SOLD . 2) (:ATIVO . 1) (:STRING . 322) (:AVAILABLE . 655))

> (get-user-by-name "User1")
((:ID . 9013685127327378666) (:USERNAME . "User1") (:FIRST-NAME . "User1FN")
 (:LAST-NAME . "USER1LN") (:EMAIL . "user1@test1.com") (:PASSWORD . "123456")
 (:PHONE . "0852538578") (:USER-STATUS . 0))
 
> (login-user "sekuritz@!47" "tevo")
((:CODE . 200) (:TYPE . "unknown")
 (:MESSAGE . "logged in user session:1613522477592"))
```