# TODO:

- board resetting
- refactor Editor to have Nothing as initial value for drawing and other
- deal with invalid drawings
- error handling on server side
- improve perfomance
    + front end?
    + remove acid state for heroku (memory managment)?
- deal with zombie touches
- add animation on drawing start (anything)
- display who currently is drawing
- Figure out why sometimes wix settings are not loaded
- check for OWNER permissions for settings pannel requests
- remove boards if noone is drawing - both clients and acid
- sequrity
    + verify instance of board on drawings delete
- post contact info to wix

#### Future:
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
- improve performance
- what to do when window dimensions greater than canvas dimensions? for now, give zoomed in.
- drop enum values from paper_type
- Do not allow same board names
- What to do with same users, and new drawing on same board by the same user? Save to local storage?
- full screen mode
- rewrite tool panel with elm