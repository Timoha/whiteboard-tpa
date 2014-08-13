# Whiteboard

A step by step description of the project is available [here](https://github.com/andreywix/whiteboard-tpa/blob/master/README.md).
Some clarifications for the description as of the current version of the application:
- No support for selecting boards to set for current widget (no "My Boards" Accordion). Only one board per widget.
- No liking/reporting/viewing name for each drawing
- No painters' names shown while they are drawing (easily implemented but doesn't worth my time right now)
- No board removing (also easily implemented)
- No settings picture as canvas background
- Board PDF can be downloaded after clicking 'Manage Drawings' in settings panel.

## [Try Out Demo](http://andreye.wix.com/test-whiteboard) on live Wix site.
If it's not loading right away, it's because Heroku turned the server to Idle. It does so after about 1 hour of no requests. Just give it 30 sec and try again.
Best working on Safari.
Works great on iPad!

# Technologies
- [Elm-lang](http://elm-lang.org) for canvas logic and jQuery for tool panel logic in widget front end
- Angular and jQuery settings panel front end
- Haskell:
    + [Scotty web framework](http://hackage.haskell.org/package/scotty) for API
    + [Wai websockets](https://hackage.haskell.org/package/wai-websockets) for Realtime
    + [Acid State](https://hackage.haskell.org/package/acid-state) as in-memory database for preserving current drawing state (will clarify below)
- PostgreSQL database

# Architecture
## Widget
- Using [Elm-lang](http://elm-lang.org) for drawing/erasing/undoing/zooming/moving around/realtime/rendering/moderating. The code can be found [here](https://github.com/andreywix/whiteboard-tpa/tree/master/public-dev/src). Everything was written from scratch. Might be slow right now, but having great progress with optimizing it.
- Using jQuery for making http requests, selecting controls of tool panel, interacting with canvas through Elm js port, as well as managing local storage for user data. The code can be found [here](https://github.com/andreywix/whiteboard-tpa/tree/master/public-dev/scripts)
- Once user visits the board, all submitted and currently in progress drawings are pulled from server.
- All events such as Drawing, Removing and Undoing are streamed to all visitors of the board
- Works great on iOS mobile devices. Android has bugs with touch event end, so I can't do anything about it.

## Settings Panel
- By clicking 'Manage drawing' user can remove inappropriate drawings in opened javascript popup (only the whole drawing at once, not parts of it). Also, user can download PDF of the board.

## Server Side
- In-progress drawings are stored in in-memory database [Acid State](https://hackage.haskell.org/package/acid-state) which sort of a dictionary. Acid state [(Atomicity, Consistency, Isolation, Durability)](http://en.wikipedia.org/wiki/ACID) allows persistence by incrementally serializing all functions with their arguments that are used to query/update acid state. So, if the server would crush for some reason, it will recover all in-progress drawings automatically. *This requires to have a file stored on the server with serialized data which for my experience is pretty small.*
- Basically acid state holds the same copy of drawings as the front end does and updates it as visitor draws. Memory consumption should not be an issue once all optimizations are done on server side (by my estimates 1GB of run could hold around 5K in progress drawings of large size). If that becomes an issue, it's easy to get rid of acid-state and just use regular Haskell objects with stm which would get rid of persistence (durability), but still allow ACI and probably smaller memory footprint.
- Drawings are submitted from acid state to Postgres when the user disconnects, or when the user clicks 'Checkmark' in canvas tool panel.
- Submitted drawings are stored in Postgres into `drawing` table as JSON arrays.
- _Database structure_ can be found [here](https://github.com/andreywix/whiteboard-tpa/blob/master/db-scheme.sql)
- Performance-wise, Haskell, once optimized appropriately, gives near C speed of execution. However, optimizing server side is still on my todo list.
- PDF is rendered from submitted strokes, and code logic is very similar to rendering front end canvas. It was kinda neat to implement that :)
- All API routes can be seen [here](https://github.com/andreywix/whiteboard-tpa/blob/master/server/Api.hs) and are pretty self-explanatory.

# Deployment
Deployment of Haskell app should be pretty easy. I've done it on Heroku with no issues using [this third party buildpack](https://github.com/begriffs/heroku-buildpack-ghc). You need in case you want to build the project on EC2. The first build can take 15-20 minutes because it has to build all dependencies. After that it takes 10-15 seconds depending on changes.

Otherwise, I imagine you could just build the app on your local machine and upload the executable to the server into main parent directory and just launch it. I imagine it shouldn't be too different from deploying Java or any other compiled language app.

In terms of using multicore processors, my app is currently single core, but because Haskell code is immutable, it very easy to make it multithreaded with all the safety to them just by using different functions.

# Current limitations
- Front end can get very slow right now (except Safari handles canvas rendering pretty well) as more drawings are added. However, I'm making good progress at optimizing it.
- Settings pannel doesn't work in safary due to wix-ui-lib not apply css.
- Back end doesn't have any security, open doors for everyone.
- No optimizations implemented for back end yet.
- Poor error handling:
   + If web socket connection break, you won't know unless you open a console. Heroku also kills the connection sometimes.
   + No error handling if something goes wrong on server.
   + If you start drawing on canvas and go outside the bounds and release the mouse, moving around and erasing will break.
- Realtime doesn't work in IE9 because it doesn't support web sockets. Only viewing will be eventually available.
- Doesn't work on Android

# Todo
My raw todo list can be viewed [here](https://github.com/andreywix/whiteboard-tpa/blob/master/TODO.md)


------------------------
In case you have any questions, my email is andrey.elenskiy(at)gmail.com.

