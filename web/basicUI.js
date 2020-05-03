//Important constants
const strategyIX = 1; 
const childsIX = 2; 
const treeMassIX = 3; 
const parentIX = 4; 
const headIX = 0; 
const viewportSize = 4000; 

// context objects
var stage; 
var playWindow; 
var ressourceWindow;
var session = pl.create(2000);
var parsed = false; 
var canvasWidth;
var canvasHeight;


var touchedObject = []; 

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

function init_Prolog() {
    // load tau
       $.get("/web/webProlog.pl", function(data) {
        parsed = session.consult(data);
        session.query("init.");
        session.answer(function (answer) {
            session.query("newTree(X).");
            session.answer();} );
    });
    
    // Tau is loaded, now pengine and during this also init tau
    pengine = new Pengine({
        oncreate: handleCreate, 
        //onsuccess: handleOutput,
        destroy: false
    });     

    console.log('Prolog Init done');
}
// here are the Pengine handle functions
function handleCreate() {
     // init tau prolog
    //session.query("state(gsntree, A).");
    //session.answer(printAnswer); 
    
    // call the init of the game at SWI Prolog
    //pengine.ask('createGame(P1, P2, Flag, Msg)');
}
// send a query to pengine = SWI Prolog = Sever Code
function sendPengine() {
    var query = $("#Tauhtml").text();
    //console.log("Query will be: " + query);
    pengine.ask(query);
}
var printAnswer = function(answer){
    // Debug code
    $("#Tauout").append(pl.format_answer(answer));
    $("#Tauout").append("<br>");
    console.log(' Tau answer:' + answer);
}

function pixiStart() 
{
    stage = new PIXI.Container();
   
    renderer = PIXI.autoDetectRenderer( {view:document.getElementById("game-canvas"), 
                                        width:1200, 
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
    
function pixiAssets() 
{
    solution = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/solution.png"].texture
    );
   
    strategy = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/strategy.png"].texture
    );
    background = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/background.png"].texture
    );
   
    canvasWidth = document.getElementById("game-canvas").width;
    canvasHeight = document.getElementById("game-canvas").height;

    // the bottom child#
    background.x = 0; 
    background.y = 0; 
    background.scale.set(2);
    stage.addChild(background);

    // generate the windows, one for the tree and one as ressource store
    playWindow = windowGenerator([{x:1, y:1, name:'play', alpha: 0.2 }, 's'],
                                        viewportSize, viewportSize, canvasWidth-200, canvasHeight); 
    ressourceWindow =  windowGenerator([ {x:0, y:1, name:'ressource', alpha: 0.4 }, ''], 
                                            200, 1000, 200, canvasHeight); 

    layerHeight = 120; 

    stage.addChild(playWindow);
    stage.addChild(ressourceWindow);

    // initial configuration
    playWindow.x = 0;
    playWindow.y = 0;
    playWindow.vpRef.scale.set(0.5);
    playWindow.vpRef.sortableChildren = true; 
    ressourceWindow.x = canvasWidth-200;
    ressourceWindow.y = 0;
    
    ressourceWindow.vpRef.dragReceiver = playWindow.vpRef; 

    stage.interactive = false;
    stage.buttonMode = false;   
  
    init_Prolog();

    // waitforParsed(500, 1);
    console.log("und weiter");
    
    session.query("showTree.");
    session.answer();

    console.log("Subtreelist at the End", subtreeList);
    requestAnimationFrame(pixiUpdate);
}

function adjustLine(line, x1, y1, x2, y2) 
{
    line.clear();  
    line.lineStyle(5, 0x5F5F5A, 10);
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
function pixiUpdate() 
{
    const elemlist =  playWindow.vpRef.children;
    
    ///////// vertical layout
    // just walk through all elements in the playWindwow viewport layer
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
           
            // take the root
            const rootSt = layerList[1][0]; // in the layer ist only one node: the goal
            const root = rootSt[headIX];

            adjustLine(elemlist[i].incomming, elemlist[i].x, elemlist[i].y, root.x, root.y);
        }
    }

    ///////// layer layout = horizontal layout

    //1-based
    for (var j = 1; j < layerList.length ; j+=2) 
    {
        const layer = layerList[j];
    
        // every subtree hangs with its head = the goal in the layer
        // every subtree has (in general) this head goal, a strategy and child goals
        for (var i = 0; i < layer.length; i++) 
        {
            const subtree = layer[i];
            const headGoal = subtree[headIX]; // head is always a goal
            const childs = subtree[childsIX];
            const headStrategy = subtree[strategyIX]; 
    
            for (var n = 0; n < childs.length; n++)
            {
                const subsubtreeID = childs[n];
                const subsubtree = subtreeList[subsubtreeID];
                const treeMass = subsubtree[treeMassIX]*0.5;
                const goal = subsubtree[headIX]; // das ist das Goal
                const strategy = subsubtree[strategyIX];

                // erste Korretur: der in der Mittelachse muss darauf bleiben
                // die Mittelachse kann sich bewegen durch layout des Layers darüber
                if (goal.v == 0) 
                {
                    goal.x = headGoal.x; 
                }
                else
                {
                    const r = (goal.x - headGoal.x);
                    var sign = Math.sign(r); 
                    const sign2 = Math.sign(goal.v);
                    // zweite Korretur: das Objekt muss auf der richtigen Seite der Achse sein
                    // nur Abstand zur Achse kann geregelt werden
                    // vorzeichen von v gibt die Seite an
                    if (sign != sign2)
                    {
                        // spieglen an der Achse
                        goal.x += sign*2*r; 
                        sign *= sign; 
                    }

                    const pedalForce =  goal.v*treeMass*160/(100*r)*sign2;
                    const attraction =  goal.mass/100*sign;  
                    var dF = pedalForce - attraction;
                    
                    if (Math.abs(dF) < 0.01)
                        dF = 0.0; 

                    goal.x += (dF)*5;
                    // if there is a strategy move it too
                    if  (!Array.isArray(strategy))
                    {
                        strategy.x = goal.x; 
                        // adjust the lin
                        adjustLine(strategy.incomming, goal.x, goal.y,strategy.x, strategy.y);
                    }
                        //console.log("r", center, element.x, r, pedalForce - attraction);
                        //console.log("r", sign, "force",pedalForce, attraction, (pedalForce -attraction), subBody[0].x);
                }
                // redraw the line
                headStrategy.x = headGoal.x
                adjustLine( goal.incomming,  headStrategy.x, headStrategy.y,goal.x, goal.y);
            }
        }
    }
   
    renderer.render(stage);
    requestAnimationFrame(pixiUpdate);
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
    //console.log("drag end");
    
    if (touchedObject.length > 0 )
    {
        const id = touchedObject[0].id;
        session.query("addStrategy("+id+").");
        session.answer(); 
        touchedObject[0].touched = false; 
        touchedObject = []; 
        playWindow.vpRef.removeChild(this);
    }

    session.query("showTree.");
    session.answer();

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

    if ((this.x < 20)  && (this.parent.name == 'ressource'))// wechseln der Fester
    {
        this.parent.dragReceiver.addChild(this); 
        this.x = 800;
        this.y = 100;
        this.dragging = true; 
    }

    //console.log(this.x, this.parent.name);
    event.stopPropagation();
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