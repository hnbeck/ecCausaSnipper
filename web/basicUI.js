//Important constants
// general constants for graphics
const viewportSize = 8000; 
const canvasSize = 1400; 
const ressourceSize = 220; 
const paletteSize = 150; 
var layerheight = 100; //default 

const cellSize = 80; //pixel

// this is the definition of basic data structure: the subtrees
const strategyIX = 1; 
const childsIX = 2; 
const treeMassIX = 3; 
const parentIX = 4; 
const levelIX = 5; 
const intervalIX = 6; 
const headIX = 0; 


// context objects
var stage; 
var playWindow; 
var ressourceWindow;
var paletteWindow; 
var session = pl.create(100000);
var parsed = false; 
var canvasWidth;
var canvasHeight;
var result; 

var touchedObject = []; 
var callBackQueue = []; 
var cbCounter = 0; 

var maxLayer; 


function waitforParsed(msec, count) 
{
    // Check if condition met. If not, re-check later (msec).
    while ((parsed !== true) && (count > 0) )
    {
        count--;
        setTimeout(function() {
            waitforParsed( msec, count);
        }, msec);
        return;
    }
    console.log("Wait is over", parsed, count);
}

//////////////////////////////// some Prolog initialization stuff ///////////////////////////////////

// function start_Prolog()
// {
//     // Tau is loaded, now pengine and during this also init tau
//     pengine = new Pengine({
//         oncreate: handleCreate, 
//         onsuccess: handleOutput,
//         destroy: false
//     });     
// }

function init_Prolog() {
    // load tau
        if (!parsed)
        {
            $.get("/web/js_prolog_interface.pl", function(data) {
                parsed = session.consult(data);
                $.get("/web/objects.pl", function(data) {
                    parsed = session.consult(data); 
                    $.get("/web/webProlog.pl", function(data) {
                        parsed = session.consult(data);
                        session.query("init.");
                        session.answer( function (answer) {
                            console.log("Init done");
                        //start_Prolog(); 
                        });
                    });
                });
            });
        }
}

//////////////////  here are the Pengine handle functions //////////////////

// function handleCreate() {

//     // call the init of the game at SWI Prolog
//     pengine.ask('createGame(8, Msg)');
// }

// Pengine handle function for reveiving SWI Prolog answer
// at the first call the initialized tree will be required

// function handleOutput()
// {
//     var resList = []; 

//     for (x in this.data[0]){
//         // properties must be lowercase in order for Tau Prolog
//         this.data[0][x.toLowerCase()] = this.data[0][x];
//         this.data[0][x].delete;
//         resList.push(x.toLowerCase());
//     }
//     // store the answer of Pengine (=JSON object) into global variable 
//     result = this.data[0];
//     //console.log("Pengine Answer", result);
    
//     // analyse the result taken from Pengine. This will result in updated
//     // knowledge base (=new predicates) in Tau Prolog
//     const querytext = "takeResult(["+ resList.toString() + "], result, Term).";
//     //console.log("Querytext", querytext);
//     session.query(querytext);
//     session.answer(executeQuery);
// }


// function sendPengine() {
//     var query = $("#Tauhtml").text();
//     //console.log("Query will be: " + query);
//     pengine.ask(query);
// }

var printAnswer = function(answer){
    // Debug code
    $("#Tauout").append(pl.format_answer(answer));
    $("#Tauout").append("<br>");
    console.log(' Tau error answer:' + answer);
}


function executeQuery(answer) 
{
    console.log("ANSWER ", answer);
    if (callBackQueue.length  == 0)
    {
        console.log("Pengine Ask");
        callBackQueue.push('startGame(X).')
       // pengine.ask('genInitalObjects(CurrentSubtree, GSNPalette)');
    }
    else
    {
        if (callBackQueue.length >0)
        {
            console.log("Tau ask");
            const query = callBackQueue.pop();
            session.query(query);
            session.answer( console.log("done:", query));
        }
    }
}

//////////////////// ////////////////// Graphics setup ////////////////////////////////

function pixiStart() 
{
    stage = new PIXI.Container();
   
    renderer = PIXI.autoDetectRenderer( {view:document.getElementById("game-canvas"), 
                                        width:canvasSize, 
                                        height:1000}
            );
   
    PIXI.loader
      .add([
        "/graphics/solution.png",
        "/graphics/strategy.png",
        "/graphics/goal.png",
        "/graphics/background2.png",
        "/graphics/windowarea.png",
        "/graphics/auge.png",
        "/graphics/hand.png",
        "/graphics/ruler.png",
        "/graphics/bt_scaleup.png",
        "/graphics/bt_scaledown.png",
        "/graphics/arrow.png",
      ])
      .load(pixiAssets);
      prio = false; 
}
    
function pixiAssets() 
{
    solution = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/solution.png"].texture
    );
   
    strategy = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/strategy.png"].texture
    );
    background = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/background2.png"].texture
    );
   
    canvasWidth = document.getElementById("game-canvas").width;
    canvasHeight = document.getElementById("game-canvas").height;

    // the bottom child#
    background.x = 0; 
    background.y = 0; 
    background.scale.set(2);
    stage.addChild(background);

    var scaleFactor = 0.8
    const windowConfigMain = {
        x:1, 
        y:1, 
        scale: scaleFactor, 
        name: 'play',
        alpha: 0.2, 
        type: 's',
        width: viewportSize, 
        height: viewportSize, 
        winWidth: canvasWidth-ressourceSize-paletteSize,
        winHeight: canvasHeight
    }

    const windowConfigRessource = {
        x:0, 
        y:1, 
        scale: 1.0, 
        name: 'ressource',
        alpha: 0.4, 
        type: 'r',
        width: ressourceSize, 
        height: 2000, 
        winWidth: ressourceSize,
        winHeight: canvasHeight
    }

     const windowConfigPalette = {
        x:0, 
        y:1, 
        scale: 1.0, 
        name: 'palette',
        alpha: 0.5, 
        type: 'r',
        width: paletteSize, 
        height: 2000, 
        winWidth: paletteSize,
        winHeight: canvasHeight
    }

    const winPosMain = new PIXI.Point(0,0);
    const winPosRessource = new PIXI.Point(windowConfigMain.winWidth + windowConfigPalette.winWidth,0);
    const winPosPalette = new PIXI.Point(windowConfigMain.winWidth,0);
    // generate the windows, one for the tree and one as ressource store
    playWindow = windowGenerator(windowConfigMain); 
    ressourceWindow =  windowGenerator(windowConfigRessource); 
    paletteWindow =  windowGenerator(windowConfigPalette); 

    layerHeight = 120; 

    stage.addChild(playWindow);
    stage.addChild(ressourceWindow);
    stage.addChild(paletteWindow);

    // initial configuration
    playWindow.x = winPosMain.x;
    playWindow.y = winPosMain.y;
    //playWindow.vpRef.scale.set(scaleFactor);
    console.log("Test", winPosRessource);
    playWindow.vpRef.sortableChildren = true; 
    ressourceWindow.x = winPosRessource.x;
    ressourceWindow.y = winPosRessource.y;
    
    paletteWindow.x = canvasWidth-ressourceSize-paletteSize;
    paletteWindow.y = 0;
   
    ressourceWindow.vpRef.dragReceiver = playWindow.vpRef; 
    paletteWindow.vpRef.dragReceiver = playWindow.vpRef; 

    stage.interactive = false;
    stage.buttonMode = false;   

    init_Prolog(); 
    
    //session.query("showTree.");
    //session.answer();

    console.log("Subtreelist at the End", subtreeList);
    requestAnimationFrame(pixiUpdate);
}

function adjustLine(line, x1, y1, x2, y2) 
{
    line.clear();  
    line.lineStyle(5, 0xCFCFCC, 10);
    line.moveTo(x1, y1);
    line.lineTo(x2, y2);

    return line; 
}
// Auftrieb dy = -k*y
// gewicht  dY = l*m
// dy = dY -> -k*y = l*m wenn y-Soll erreicht  
// y soll ist Level*100+50 = L*B + B/2 = l*B*1.5
// l = 1/100 -> dY = 10
// -k*y = 10 -> ys = 10/k = L*B*1.5; -> k = 2*10/3LB = 20/(3*L*B)

// anziehung e*r= mv^2/r <=> e*r^2 = v*v <=> r =v * e'
// 100 = 10*10
// 200 = 20 * 10 
function placeVertical(aWindow)
{
    const elemlist =  aWindow.children;

    for (var i = 0; i < elemlist.length; i++)
    {
         if (elemlist[i].touched == true)
                elemlist[i].scale.set(1.2);
         else
                elemlist[i].scale.set(1);

        if ((elemlist[i].mass > 0) && (elemlist[i].dragging !== true))
        {
            // force down
            const gForce  = elemlist[i].mass/10;
            const updrive = elemlist[i].k*elemlist[i].y;

            if (gForce - updrive  >= 0.1)
            {
                elemlist[i].y += updrive+gForce; 
            }
           
            if ((elemlist[i].name != 'solution'))
            {

                const rootSt = layerList[1][0]; // in the layer ist only one node: the goal
                const root = rootSt.goal;

                adjustLine(elemlist[i].incomming, elemlist[i].x, elemlist[i].y, root.x, root.y);
            }
            // take the root
            elemlist[i].autoMove(); 
        }
    }
}

function pixiUpdate() 
{
  
    placeVertical(playWindow.vpRef);
    placeVertical(ressourceWindow.vpRef);
    placeVertical(paletteWindow.vpRef);
    

    ///////// layer layout = horizontal layout
    var delta1; 
    var delta2; 

    for (var j = 1; j < layerList.length ; j+=2) 
    {
        const layer = layerList[j];

        for (var i = 0; i < layer.length; i++) 
        {
            const subtree = layer[i];

            if (subtree != null)
            {
                var headStrategy;
                var parentSubtree; 

                if (subtree.parentID != 'root')
                {
                    parentSubtree = subtreeList[subtree.parentID]; 
                    headStrategy = parentSubtree.strategy;
                }
               
                iv = subtree.interval;
                const iv2 = shiftScaleInterval(viewportSize/2, cellSize, subtree.interval);

                delta2 = (iv2[1]-iv2[0])/2 + iv2[0] - subtree.goal.x;
                subtree.goal.x += delta2/20 ;  //animation
               
                if  (!Array.isArray(subtree.strategy))
                {
                   subtree.strategy.x = subtree.goal.x; 
                    // adjust the lin
                   adjustLine(subtree.strategy.incomming, 
                                subtree.goal.x, 
                                subtree.goal.y, 
                                subtree.strategy.x, 
                                subtree.strategy.y);
                }

             
                if (subtree.parentID != 'root')
                {
                    if (!Array.isArray(headStrategy)) 
                    {// redraw the line
                        adjustLine( subtree.goal.incomming,  
                                    headStrategy.x, 
                                    headStrategy.y, 
                                    subtree.goal.x, 
                                    subtree.goal.y);
                    }
                }

            }

        }
        // for (var i = 0; i < space.length; i++) 
        // {
        //     space[i].x += space[i].delta;
        //     space[i].delta = 0; 
        // }


    }

    renderer.render(stage);
    requestAnimationFrame(pixiUpdate);
}

 

function shiftScaleInterval(x, scale, iv)
{
    x1 = iv[0]*scale + x; 
    x2 = iv[1]*scale + x;

    return [x1, x2];
}

/////////////////////////////////// Event Handling ///////////////////////////////////////////////
// collision with a goal 
// object1 hast to be a goal, object 2 is the dragged object
function collisionCheck(object1) 
{   
    if ((object1.name == "goal") && (object1.id != this.id))
    {
        const bounds1 = object1.getBounds();
        const bounds2 = this.getBounds();

        object1.touched = false; 

        return bounds1.x < bounds2.x + bounds2.width
            && bounds1.x + bounds2.width > bounds2.x
            && bounds1.y < bounds2.y + bounds2.height
            && bounds1.y + bounds2.height > bounds2.y;
    }
    else
    {
        object1.touched = false; 
        return false; 
    }
}


function onDragStart(event) {
    // store a reference to the data
    // the reason for this is because of multitouch
    // we want to track the movement of this particular touch
    console.log("DRAG START", event.data)
    this.data = event.data;
    this.alpha = 0.5;
    this.dragging = true;

    prio = true; 

    event.stopPropagation();
}

function onDragEnd() 
{
    this.alpha = 1;
    this.dragging = false;
    this.data = null;
    prio = false; 
    console.log("drag end", this.name);

    if ((touchedObject.length > 0 ) && (touchedObject[0].receptor))
    {
        const id = touchedObject[0].id;
        const draggedID = this.id; 
        touchedObject[0].touched = false; 
       
        if (this.name == 'strategy')
        {
            const query = 'addGoalChild(strategy,' + id +', '+ draggedID;
            callBackQueue.push(query + ').');
            //console.log("an Pengine", query);
            //pengine.ask(query + ', CurrentSubtree)');
            playWindow.vpRef.removeChild(this);
            touchedObject[0].receptor = false; 
            touchedObject = [];  

        }

        if (this.name == 'solution')
        {
            const query = 'addGoalChild(solution,' + id +', '+ draggedID ;
            callBackQueue.push(query + ').');
            //console.log("An Pengine ", query);
            //pengine.ask(query + ', CurrentSubtree)');
            const n = solutionList.indexOf(this);
            playWindow.vpRef.removeChild(this);
            solutionList.splice(n, 1);
            touchedObject[0].receptor = false; 
            touchedObject = [];  
        }
       
    }

    if (this.linked == false)
        this.autoMove = moveHomeFunc;

    // hier Tau Aufrfen
    event.stopPropagation();
}

function onDragMove() 
{
    if (this.dragging)
    {
        const newPosition = this.data.getLocalPosition(this.parent);
        this.x = newPosition.x;
        this.y = newPosition.y;

        const elemlist =  playWindow.vpRef.children;
        touchedObject = elemlist.filter(collisionCheck, this);

        if (touchedObject.length > 0) 
        {
            touchedObject[0].touched = true; 
        }
    }

    if (leaveRessource(this))// wechseln der Fester
    {
        var newPos = transposeCoord(this);
        this.parent.dragReceiver.addChild(this); 
        this.borderCrossX = newPos.x;
        this.borderCrossY = newPos.y;
        this.dragging = true; 
    }

    if (leavePalette(this))// wechseln der Fester
    {
        var newPos = transposeCoord(this);
        this.parent.dragReceiver.addChild(this); 
        this.dragging = true; 
        this.borderCrossX = newPos.x;
        this.borderCrossY = newPos.y;
    }
    //console.log(this.x, this.parent.name);
    event.stopPropagation();
}

function transposeCoord(aPixiObject)
{
    parent = parentViewport(aPixiObject);
    origin = new PIXI.Point(0,0);
    var newPos = parent.dragReceiver.toLocal(origin, aPixiObject);

    if (parent.name == 'palette' )
         newPos = parent.dragReceiver.toLocal(origin, parent.x);
    // depending on the window a correction is needed
    if (parent.name == 'ressource' )
         newPos = parent.dragReceiver.toLocal(origin, parent.x-paletteSize);

    return newPos; 
}

function parentViewport(aPixiObject)
{
    return aPixiObject.parent;

}

function leaveRessource(aPixiObject)
{
    const parent = parentViewport(aPixiObject);

    if ((aPixiObject.x < 20)  && (parent.name == 'ressource'))
        return true; 
    else
        return false; 
}

function leavePalette(aPixiObject)
{
    const parent = parentViewport(aPixiObject);

    if ((aPixiObject.x < 20)  && (parent.name == 'palette'))
        return true; 
    else
        return false; 
}

function moveHomeFunc() 
{
    if (this.dragging)
        return; 

    const deltaX = this.borderCrossX - this.x;
    const deltaY = this.borderCrossY - this.y;   

    if ((deltaX == 0) && (deltaY == 0) )
        return

    //console.log("Aufgerufen", this.borderCrossX, deltaX, deltaY);

    if (Math.abs(deltaX) > 2)
        this.x += deltaX/10 ; 
    else 
        this.x = this.borderCrossX;

    if (Math.abs(deltaY) > 2)
        this.y += deltaY/10 ; 
    else
        this.y = this.borderCrossY;
}
  

function moveNeutralFunc() 
{

}

// function pointerMove(event) {
//     if (this.dragging && !prio ) 
//     {
//             const newPosition = this.data.global;
//             aNew.x = newPosition.x;
//             aNew.y = newPosition.y;
//     }
// }

// function pointerDown(event) {
   
//     console.log("Back");

//     if (!prio)
//     {
//         aNew = strategyGenerator([[2, 'rl'],[1, 'mt'], [3, 'ph']]);
//         this.data = event.data;
//         aNew.alpha = 0.5;
//         aNew.pivot.x = aNew.width / 2;
//         aNew.pivot.y = aNew.height / 2;
//         this.dragging = true;

//         const newPosition = this.data.global; 
//         aNew.x = newPosition.x; 
//         aNew.y = newPosition.y; 
//         stage.addChild(aNew);
//     }
// }

// function pointerUp(event) {
//     if (this.dragging && !prio )
//     {
//         this.dragging = false;
//         aNew.alpha = 1.0; 
//     }
// }