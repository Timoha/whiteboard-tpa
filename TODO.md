# TODO:

Thursday:
- improve perfomance
- remove acid state for heroku?
- add tooltips
- add animation on drawing start (anything)
###### Ready to show to Wix SF
- display who currently is drawing
- Figure out why sometimes wix settings are not loaded

Friday:
- Generate PDF from strokes
- Manage Board, open editing in js popup
- board resetting
- local strage
- Refactor to be able to submit drawings automatically
- check for OWNER permissions for settings pannel requests
- sequrity
    + verify instance of board on drawings delete
- post contact info to wix

#### Future:
- fix email validation
- min zoom to view the whole board
- disconnection handling - think of something
- Remove drawing signal for handling disconnection?
- refactor server signals
- liking/reporting drawings
- creating new boards
- Pull other boards of this instance and be able to set them to current
- Point is also a stroke, might have a prettier way to write logic
- improve performance
- refactor moderation (create a separate module instead of adding it to Editor?)
- what to do when window dimensions greater than canvas dimensions? for now, give zoomed in.
- drop enum values from paper_type
- Do not allow same board names
- What to do with same users, and new drawing on same board by the same user? Save to local storage?
- full screen mode
- rewrite tool panel with elm