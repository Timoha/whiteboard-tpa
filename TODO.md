# TODO:

- Validate each socket request and broadcast only to current board
- Pull drawings from database and set them on init in widget
- Do not allow to select smaller paper sizes if board is not empty
- board reset
- board moderation and resetting if current, deleting if other
- validate email and name on drawing start
- Generate PDF from strokes
^^^^^^^^ Over the weekend ^^^^^^^^
- displaying who is currently drawing
- Use acid-state to store current drawing progress
- min zoom to view the whole board
- liking/reporting drawings
- creating new boards
- Pull other boards of this instance and be able to set them to current
- post contact info to wix

#### Future:
- what to do when window dimensions greater than canvas dimensions? for now, give zoomed in.
- drop enum values from paper_type
- Do not allow same board names
- What to do with same users, and new drawing on same board by the same user? Save to local storage?