//Create a Pixi Application
var stage; 
var playWindow; 
var ressourceWindow;
var session = pl.create(1000);
var parsed = null; 
var layerheight; 
var layerlist = []; 
var bodylist =  []; 
var maxLayer; 

async function init_Prolog() {
    // load tau
     await $.get("/web/webProlog.pl", function(data) {
        parsed = session.consult(data);
        session.query("init.");
        session.answer(printAnswer);
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
    session.query("state(gsntree, A).");
    session.answer(printAnswer); 
    
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

function pixiStart() {

    init_Prolog();


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

    
    var sCont  = gsnElemGenerator('strategy',0, [[2, 'rl'],[3, 'mt'], [1, 'ph']]);

    layerheight = sCont.height*1; 
    //var sSolution  = gsnElemGenerator('solution',[[1, 'rl'],[0, 'mt'], [0, 'ph']]);
   
    playWindow = windowGenerator([{x:1, y:1, name:'play', alpha: 0.2 }, 's'],
                                        3000, 3000, canvasWidth-200, canvasHeight); 
    ressourceWindow =  windowGenerator([ {x:0, y:1, name:'ressource', alpha: 0.4 }, ''], 
                                            200, 1000, 200, canvasHeight); 

    stage.addChild(playWindow);
    stage.addChild(ressourceWindow);

    playWindow.x = 0;
    playWindow.y = 0;
    playWindow.vpRef.scale.set(0.7);
    ressourceWindow.x = canvasWidth-200;
    ressourceWindow.y = 0;


    sCont.x = 100;
    sCont.y = 150; 

    // vprRef is the reference to the viewport where all elements resides (are child of)
    ressourceWindow.vpRef.addChild(sCont);
    ressourceWindow.vpRef.dragReceiver = playWindow.vpRef; 

    stage.interactive = false;
    stage.buttonMode = false; 
  
    const rootGoal = [1, [[2, 'rl'], [1, 'mt'], [0, 'ph']], 1];
    const strg = [2, [[2, 'rl'], [1, 'mt'], [0, 'ph']], 2]; 
    const strg2 = [5, [[2, 'rl'], [2, 'mt'], [2, 'ph']], 4]; 

    const g2 = [2, [[1, 'rl'], [0, 'mt'], [4, 'ph']], 3];
    const g3 = [3, [[1, 'rl'], [0, 'mt'], [0, 'ph']], 3];
    const g4 = [4, [[3, 'rl'], [1, 'mt'], [1, 'ph']], 3];

    const g5 = [7, [[1, 'rl'], [0, 'mt'], [4, 'ph']], 5];
    const g6 = [8, [[1, 'rl'], [0, 'mt'], [0, 'ph']], 5];
    const g7 = [9, [[3, 'rl'], [1, 'mt'], [1, 'ph']], 5];

    
    const emptybody2 = [g2, [], []]; 
    const emptybody4 = [g4, [], []]; 
    const emptybody5 = [g5, [], []]; 
    const emptybody6 = [g6, [], []]; 
    const emptybody7 = [g7, [], []]; 
    const subtree = [g3, strg2, [ emptybody5,  emptybody6, emptybody7 ]];

    embodySubtree([], canvasWidth/3, layerheight, 
                    playWindow.vpRef, 
                    [rootGoal, strg, [emptybody2,  subtree , emptybody4]]);

    background.x = 0; 
    background.y = 0; 

    console.log("Layerlist ", layerlist);
    requestAnimationFrame(pixiUpdate);
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
function pixiUpdate() {

    var elemlist =  playWindow.vpRef.children;
    var line; 

    for (var i = 0; i < elemlist.length; i++)
    {
        if (elemlist[i].mass > 0)
        {
            // force down
            const gForce  = elemlist[i].mass/100;
            const updrive = elemlist[i].k*elemlist[i].y;

            if (gForce - updrive  >= 0.1){
                elemlist[i].y += updrive+gForce; 
            }
        }
    }

    //1-based
    //for (var j = layerlist.length-1; j > 0; j-=2) 
    for (var j = 1; j < layerlist.length ; j+=2) 
    {
        const layer = layerlist[j];
       
        // im layer sind jetzt subtrees, eigentlich subtree Köpfe enthalten

        for (var i = 0; i < layer.length; i++) 
        {
            const body = layer[i];
            const id = body[0].id;     
            const parenty = body[0].y;
            const parentx = body[0].x;
            const childs = bodyChilds(id);
            const parentstg = body[1]; 
            
            if (childs.length>0) 
            {
                const center = bodyMid(id);

                for (var n = 0; n < childs.length; n++)
                {

                    const subBody = childs[n];
                    const bodymass = subBody[3];
                    const element = subBody[0]; // das ist das Goal
                    const strategy = subBody[1];

                    // erste Korretur: der in der Mittelachse muss darauf bleiben
                    // die Mittelachse kann sich bewegen durch layout des Layers darüber
                    if (element.v == 0) 
                    {
                        element.x = center; 
                       
                    }
                    else
                    {
                        const r = (element.x - center);
                        var sign = Math.sign(r); 
                        const sign2 = Math.sign(element.v);
                        // zweite Korretur: das Objekt muss auf der richtigen Seite der Achse sein
                        // nur Abstand zur Achse kann geregelt werden
                        // vorzeichen von v gibt die Seite an
                        if (sign != sign2)
                        {
                            // spieglen an der Achse
                            element.x += sign*2*r; 
                            sign *= sign; 
                        }


                        const pedalForce =  element.v*bodymass*160/(1000*r)*sign2;
                        const attraction =  element.mass/1000*sign;  
                        //console.log(element.v, element.mass, element.id, r);
                        if (Math.abs(pedalForce - attraction) > 0.01)
                        {
                            element.x += (pedalForce - attraction)*5;
                            if  (!Array.isArray(strategy))
                            {
                                strategy.x = element.x; 

                                line = strategy.incomming; 
                                line.clear(); 
                                line.lineStyle(5, 0x5F5F5A, 10);
                                line.moveTo(element.x, element.y);
                                line.lineTo(strategy.x, strategy.y);
                            }
                            //console.log("r", center, element.x, r, pedalForce - attraction);
                            //console.log("r", sign, "force",pedalForce, attraction, (pedalForce -attraction), subBody[0].x);
                        }
                        else
                        {
                            element.x = center + r; 
                        }

                        
                       
                    }
                    // redraw the line
                    line = element.incomming; 
                    line.clear(); 
                    line.lineStyle(5, 0x5F5F5A, 10);
                    line.moveTo(parentstg.x, parentstg.y);
                    line.lineTo(element.x, element.y);
    
                }
                    
            }
            
        }
    }
   
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