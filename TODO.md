# TODO:

- error handling on server side
- security
    + verify instance of board on drawings delete
    + check for OWNER permissions for settings pannel requests
- ping-pong for preserving websockets connection
- deal with zombie touches
- show minimap always in bottom left corner
- improve perfomance
    + moderator - apply same technique as in editor

#### Future:

- trim painters input text on signup
- post contact info to wix
- board resetting
- if background is transparent and white, make pdf background light gray. or make white light gray
- add tooltips
- min zoom to view the whole board
- click on minimap to change location
- disconnection handling - think of something
- Remove drawing signal for handling disconnection?
- refactor server signals
- liking/reporting drawings
- creating new boards
- Pull other boards of this instance and be able to set them to current
- Point is also a stroke, might have a prettier way to write logic
- what to do when window dimensions greater than canvas dimensions? for now, give zoomed in.
- Do not allow same board names
- What to do with same users?
- rewrite tool panel with elm
- full screen mode
