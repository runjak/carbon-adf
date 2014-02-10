module Carbon.Website.Crud(
  crudCreate
, crudRead
, crudUpdate
, crudDelete
, respOk
, respCreated
, respNotModified
, respBadRequest
, respUnauthorized
, respForbidden
, respNotFound
, respInternalServerError
)where
{-|
  Inspired by http://devo.ps/blog/2013/03/22/designing-a-restful-api-that-doesn-t-suck.html
|-}
import Carbon.Website.Monad as Monad

crudCreate = decode $ method POST
crudRead   = decode $ method GET
crudUpdate = decode $ method PUT
crudDelete = decode $ method DELETE

decode :: OBW () -> OBW a -> OBW a
decode check = (>>) $ check >> decodeBody (defaultBodyPolicy "/tmp/" 1000 1000 1000)

{-
200: OK
201: Created
304: Not Modified
400: Bad Request
401: Unauthorized
403: Forbidden
404: Not Found
500: Internal Server Error
-}
respOk                  = resp 200 :: Response -> OBW Response
respCreated             = resp 201 :: Response -> OBW Response
respNotModified         = resp 304 :: Response -> OBW Response
respBadRequest          = resp 400 :: Response -> OBW Response
respUnauthorized        = resp 401 :: Response -> OBW Response
respForbidden           = resp 403 :: Response -> OBW Response
respNotFound            = resp 404 :: Response -> OBW Response
respInternalServerError = resp 500 :: Response -> OBW Response
