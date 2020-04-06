//Create a Pixi Application
var stage; 
var playWindow; 

function pixiStart() {

    stage = new PIXI.Container();
   
    renderer = PIXI.autoDetectRenderer( {view:document.getElementById("game-canvas"), 
                                        width:1000, 
                                        height:800}
            );
   
    PIXI.loader
      .add([
        "/graphics/solution.png",
        "/graphics/strategy.png",
        "/graphics/goal.png",
        "/graphics/background.png",
        "/graphics/windowarea.png",
        "/graphics/auge.png",
        "/graphics/hand.png",
        "/graphics/ruler.png",
        "/graphics/bt_scaleup.png",
        "/graphics/bt_scaledown.png",
      ])
      .load(pixiAssets);
      prio = false; 
}
    
function pixiAssets() {
    solution = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/solution.png"].texture
    );
   
    strategy = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/strategy.png"].texture
    );
    background = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/background.png"].texture
    );
   
    const canvasWidth = document.getElementById("game-canvas").width;
    const canvasHeight = document.getElementById("game-canvas").height;

    // the bottom child
    stage.addChild(background);

    var goalCont  = gsnElemGenerator('goal',[[2, 'rl'],[1, 'mt'], [0, 'ph']]);
    var sCont  = gsnElemGenerator('strategy',[[2, 'rl'],[3, 'mt'], [1, 'ph']]);
    var sSolution  = gsnElemGenerator('solution',[[1, 'rl'],[0, 'mt'], [0, 'ph']]);
   
    var playWindow = windowGenerator([{x:1, y:1, name:'play', alpha: 0.2 }, 's'],
                                        1000, 2000, canvasWidth-200, canvasHeight); 
    var ressourceWindow =  windowGenerator([ {x:0, y:1, name:'ressource', alpha: 0.4 }, ''], 
                                            200, 1000, 200, canvasHeight); 

    stage.addChild(playWindow);
    stage.addChild(ressourceWindow);

    playWindow.x = 0;
    playWindow.y = 0;
    ressourceWindow.x = canvasWidth-200;
    ressourceWindow.y = 0;

   
    // initiale Goals
    goalCont.x = 100;
    goalCont.y = 50; 
    sCont.x = 100;
    sCont.y = 150; 

    playWindow.vpRef.addChild(goalCont);
    playWindow.vpRef.addChild(sSolution);
    sSolution.x = ressourceWindow.width/2-sSolution.width/2; 
    sSolution.y = 100;  

    // vprRef is the reference to the viewport where all elements resides (are child of)
    ressourceWindow.vpRef.addChild(sCont);
    ressourceWindow.vpRef.dragReceiver = playWindow.vpRef; 

    stage.interactive = false;
    stage.buttonMode = false; 
   // stage.on('mousedown', pointerDown);
  //  stage.on('mouseup', pointerUp);
   // stage.on('mousemove', pointerMove);
    

    background.x = 0; 
    background.y = 0; 

    requestAnimationFrame(pixiUpdate);
}


// type list is of format [[number, type], [number, type]....]
function gsnElemGenerator(ElemType, TypeList) {

    const textMargin = 5;
    const elemCont = new PIXI.Container();

    elemCont.interactive = true; 
    // Text style for numbers
    const style = new PIXI.TextStyle({
        fontFamily: 'Arial',
        fontSize: 14,
        fill: '#FFFFFF'
    });

    var ressourceID;
    var correction = 0; 

    switch(ElemType)
    {
        case ('strategy'): 
            ressourceID = "/graphics/strategy.png"; 
            correction = 5; 
        break;

        case ('solution'):
             ressourceID = "/graphics/solution.png"; 
        break

        case ('goal') :
             ressourceID = "/graphics/goal.png"; 
        break; 

    }
   
    const gsnElement = new PIXI.Sprite(
                PIXI.loader.resources[ressourceID].texture
            );  
    elemCont.addChild(gsnElement);
    elemCont.name = ElemType; 
    gsnElement.anchor.set(0.5);
    elemCont 
            .on('pointerdown', onDragStart)
            .on('pointerup', onDragEnd)
            .on('pointerupoutside', onDragEnd)
            .on('pointermove', onDragMove);


    const shiftX = gsnElement.width/2; 
    const shiftY = gsnElement.height/2; 

    var pixiObjects = TypeList.map(symbolGenerator);
    var layout = calcLayout(gsnElement);

    for (var i = 0; i < 3; i++)
    {
        const number = TypeList[i][0];
        const typeID = TypeList[i][1];

        const element = pixiObjects[i];
        const basicText = new PIXI.Text(number.toString(), style);
        basicText.anchor.y = 1.0; 
        basicText.x = layout[typeID][0] + correction - shiftX;
        basicText.y = layout[typeID][1] - shiftY;
        elemCont.addChild(basicText);
       
        element.x = basicText.x + basicText.width + textMargin;
        element.y = basicText.y;
       
        elemCont.addChild(element);
    }
    return elemCont;
}

function calcLayout(element) {

    var layout;

    const w = element.width; 
    const h = element.height; 
    const margin = element.height / 4;

    const midX = w/2; 
    const midY = h/2;

    var layout = {  rl: [margin, h - margin], 
                    mt: [midX, h - margin],
                    ph: [midX, midY]}
    return layout; 
}
// generates a pixi object according the type
// elem is of format [number, type]
function symbolGenerator(elem) {

    const typeID = elem[1]; 
    var type; 

    switch(typeID) {

        case 'rl': 
            type = new PIXI.Sprite(
                PIXI.loader.resources["/graphics/hand.png"].texture
            );
            type.scale.set(0.3);
            break; 
        case 'mt':
            type = new PIXI.Sprite(
                PIXI.loader.resources["/graphics/ruler.png"].texture
            );
            type.scale.set(0.4);
            break; 
        case 'ph':
            type = new PIXI.Sprite(
                PIXI.loader.resources["/graphics/auge.png"].texture
            );
            type.scale.set(0.4);
    }   
    type.anchor.y = 1.0; 

    return type;
}

function pixiUpdate() {

    //sSprite.x += 0.1;

    renderer.render(stage);
    requestAnimationFrame(pixiUpdate);
}


/////////////////////////////////// Event Handling ///////////////////////////////////////////////

function onDragStart(event) {
    // store a reference to the data
    // the reason for this is because of multitouch
    // we want to track the movement of this particular touch
    this.data = event.data;
    this.alpha = 0.5;
    this.dragging = true;
    prio = true; 

    console.log("Sprite");
    event.stopPropagation();

}

function onDragEnd() {
    this.alpha = 1;
    this.dragging = false;
    // set the interaction data to null
    this.data = null;
    prio = false; 
    //console.log("drag end");
    event.stopPropagation();
}

function onDragMove() {
    if (this.dragging) {
        const newPosition = this.data.getLocalPosition(this.parent);
        this.x = newPosition.x;
        this.y = newPosition.y;
    }
    if (this.x < 20)
    {
        this.parent.dragReceiver.addChild(this); 
        this.x = 800;
        this.y = 100;
        this.dragging = true; 
    }
    //console.log(this.x, this.parent.name);
    event.stopPropagation();
}


function pointerMove(event) {
    if (this.dragging && !prio ) {
            const newPosition = this.data.global;
            aNew.x = newPosition.x;
            aNew.y = newPosition.y;
    }
}

function pointerDown(event) {
   
    console.log("Back");

    if (!prio)
    {
        aNew = strategyGenerator([[2, 'rl'],[1, 'mt'], [3, 'ph']]);
        this.data = event.data;
        aNew.alpha = 0.5;
        aNew.pivot.x = aNew.width / 2;
        aNew.pivot.y = aNew.height / 2;
        this.dragging = true;

        const newPosition = this.data.global; 
        aNew.x = newPosition.x; 
        aNew.y = newPosition.y; 
        stage.addChild(aNew);
    }
}

function pointerUp(event) {
    if (this.dragging && !prio )
    {
        this.dragging = false;
        aNew.alpha = 1.0; 
    }
}