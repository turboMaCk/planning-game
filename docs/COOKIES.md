# Cookies Usage

This project uses cookies solely for purpose of providing functionality.
Cookies are used to persist session ids to pair sessions across browser
tabs or windows and between refreshes of browser which significantly improves user experience.

Cookies are used over local storage or other persistent APIs provided by browser
because of better security guarantees and larger support.

Server itself reads cookies only to authorize websocket connection
because browser API for websockets doesn't allow usage of custom HTTP headers.

XHR requests are authenticated via `Authorization` HTTP header.

## List of Cookies

This is the list of cookies used by the application:

| Name            | value type | Expiration | Usage                                                               |
| --------------- | ---------- | ---------- | ------------------------------------------------------------------- |
| cookiesApproved | Boolean    | 30 days    | Persists state for confirmation of cookies usage                    |
| sessionId       | String     | never      | Stores session id used to match browser to player in scope of table |

Implementation can be reviewed in [init.js](../public/init.js)
