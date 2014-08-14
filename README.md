# Whiteboard
WiX.com App which allows visitors to leave drawings on the website.
### [Try Live Demo] (http://andreye.wix.com/wix-sf-whiteboard)

### Viewing Mode:

#### Default state (first thing that visitors see)
- All drawings are real-time. Painter's name is showing where he/she is currently drawing

![default](https://github.com/andreywix/whiteboard-tpa/raw/master/wireframes/default.png)

#### When user clicks on one of the drawings
- Visitors can like or report a drawing which they clicked on

![drawing click](https://github.com/andreywix/whiteboard-tpa/raw/master/wireframes/click-drawing.png)

#### When user clicks on "Brush" to start drawing
- Once user clicks on "Start Drawing", a Drawing Mode will be activated
- Visitor's name and email will be stored into WiX contacts

![start drawing](https://github.com/andreywix/whiteboard-tpa/raw/master/wireframes/start-drawing-panel.png)

### Drawing Mode - [Try Live Demo of Editor](http://andreywix.github.io/whiteboard-tpa/public-dev/):
- Click onto "Checkmark" when done with the drawing
- Can access Viewing Mode functionality by pressing "Hand"
- People's names will be shown only in "Hand" mode (maybe)

#### Default "Drawing Mode" state (first thing that visitors see)
![default](https://github.com/andreywix/whiteboard-tpa/raw/master/wireframes/selected.png)

#### When clicked on "Brush Dot" color panel opens
- The dot will change accordingly once the visitor changes Radius/Color/Opacity

![color panel](https://github.com/andreywix/whiteboard-tpa/raw/master/wireframes/animated/color-panel.gif)

#### When clicked on the "Hand"
- "Drag Mode" will be activated and current location will be shown on the map

![map panel](https://github.com/andreywix/whiteboard-tpa/raw/master/wireframes/map-panel.png)


### Settings Panel - [Try live demo of Settings Panel](http://andreywix.github.io/whiteboard-tpa/public-dev/settings.html):

#### General settings
- User can set a number of visitor's reports a particular drawing can get before it gets removed
- "+ Start New Board" creates an empty board in "My Boards" ([see below](https://github.com/andreywix/whiteboard-tpa/blob/master/README.md#my-boards)) and sets it to current

![general settings](https://github.com/andreywix/whiteboard-tpa/raw/master/wireframes/general-settings.png)

#### Canvas settings
- Printing size is the size which allows the drawings to be scaled appropriatly. The brush radius will always be the of the same size relative to physical world. For example, if brush size is 8px, the final drawing will be of the same size regardles on what size of paper it will be printed on.
- Canvas background picture will be uploaded via Media Gallery

![canvas settings](https://github.com/andreywix/whiteboard-tpa/raw/master/wireframes/canvas-settings.png)

#### Tool Panel settings
![tool panel settings](https://github.com/andreywix/whiteboard-tpa/raw/master/wireframes/panel-settings.png)

#### My Boards
- Allows to view all boards and create a fresh board or choose another one on place of current widget. All settings of that board will be set to currrent (might think about this more)

![my boards](https://github.com/andreywix/whiteboard-tpa/raw/master/wireframes/boards-settings.png)

When clicked on "eye" to view board, a popup opens which allows to:
- manually remove unwanted drawings with eraser tool
- download PDF of the board
- set the board to the current widget
- completely deleting the board

![board popup](https://github.com/andreywix/whiteboard-tpa/raw/master/wireframes/animated/popup.gif)
