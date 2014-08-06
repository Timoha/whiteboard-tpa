# TODO:

- delete drawing if disconnected and not submitted? Nope, always keep it <- Way to cancel drawing (Maybe for now)
- validate email and name on drawing start, add some animation?
- Figure out why sometimes wix settings are not loaded
- heroku deployment
- board resetting
^^^^^^^ Wednesday ^^^^^^^^
- min zoom to view the whole board
- Generate PDF from strokes
- display who currently is drawing
- check for OWNER permissions for settings pannel requests
- post contact info to wix
- add tooltips

#### Future:
- Remove drawing signal for handling disconnection?
- refactor server signals
- liking/reporting drawings
- creating new boards
- Pull other boards of this instance and be able to set them to current
- Point is also a stroke, might have a prettier way to write logic
- verify instance of board on drawings delete
- improve performance
- refactor moderation (create a separate module instead of adding it to Editor?)
- board's design field to json
- what to do when window dimensions greater than canvas dimensions? for now, give zoomed in.
- drop enum values from paper_type
- Do not allow same board names
- What to do with same users, and new drawing on same board by the same user? Save to local storage?
- full screen mode
- rewrite tool panel with elm