
// subtree = body
// [goal, strategy, [subtrees]]
// das müssen die Elemente der Layer sein
// goal ist der Kopf des Subtrees

// layer is a list of subtrees, each subtree has a body
// layerlist = [subtree1, subtree2....], with index = layer no
var layerList = []; 

// subtreeList is a list of all subtrees, 
// describing the embodyment of a subtree
// subtree = [goal, strategy, [Subtrees], mass], this are the PIXI objects
// index in subtreeList = goal.id; 
var subtreeList = []; // is in old code the body[]

var layerheight = 100; //default 

// einige wichtige Indizes
const iMass = 0; 
const iKFact = 1; 
const iV = 2; 

// this function will be replaced by Prolog
// function embodySubtree(body, x,y, container, subtree){

//     const goal = subtree[0];
//     const strategy = subtree[1];
//     const subsubtree = subtree[2];

//     const glName = goal[0];
//     const glAttributes = goal[1];
//     const glLayer = goal[2];
//     var line; 

//     // masse ist eine strukturelle Größe wird zur eigenschaft

//     const goalCont  = gsnElemGenerator('goal', glName, glAttributes);
//     goalCont.x = x; 
//     goalCont.y = y; 
//     goalCont.level = glLayer; 
//     goalCont.k = kFactor(glLayer, layerheight);
//     goalCont.id = glName; 
   
  
//     body[0] = goalCont; 
//     body[1] = []; 
//     body[2] = []; 
//     body[3] = 0; 

//     //container.addChild(goalCont);
//     var strategyCont; 

//     if (strategy.length > 0){
//         const stName = strategy[0];
//         const stAttributes = strategy[1];
//         const stLayer = strategy[2];

//         strategyCont  = gsnElemGenerator('strategy', stName, stAttributes);
//         strategyCont.x = x; 
//         strategyCont.y = y + 5; 
//         strategyCont.level = stLayer; 
       
//         strategyCont.k = kFactor(stLayer, layerheight);
       
       
//         line = new PIXI.Graphics();
//         line.name = "e";
//         // jetzt noch die Linie
//         line.lineStyle(5, 0x5F5F5A, 10);
//         line.moveTo(goalCont.x, goalCont.y);
//         line.lineTo(strategyCont.x, strategyCont.y);
//         container.addChild(line);
//         strategyCont.incomming = line;

//         body[1] = strategyCont; 
//         container.addChild(strategyCont);
//     }
    
//       container.addChild(goalCont);

//     var subtreeList = []; 
//     var dx = [0, 10, -10, 15, -15, 20, -20, 25, -25, 30, -30];
//     var v = [0, 1, -1, 2, -2, 3, -3, 4, -4, 5, -5];

//     for (var i = 0; i < subsubtree.length; i++) {
        
//          line = new PIXI.Graphics();
//           container.addChild(line);
//         //console.log(subsubtree);
//         const subBody = embodySubtree([], x+dx[i], y + 20, container, subsubtree[i]);
//         subBody[0].v = v[i];  //
//         subtreeList[i] = subBody.slice();
//         body[3] += subBody[0].mass;  
        
       
//         line.name = "e";
//         line.lineStyle(5, 0x5F5F5A, 10);
//         line.moveTo(strategyCont.x, strategyCont.y);
//         line.lineTo(subBody[0].x, subBody[0].y);
       
//         subBody[0].incomming = line; 
//     }

//     if (body[3] == 0) {
//         body[3] = goalCont.mass; 
//     } else
//     {
//         body[3] *= 0.9; 
//     }

//     body[2] = subtreeList; 
//     bodylist[goalCont.id]  = body; 
   
//     add2LayerList(glLayer, body);

//     return body; 

// }

// subtree kommt von Prolog als Liste, fast wie der fertige Body
// [['goal', ID, body, explanation], ['strategy', ID, Body, explanation], [...] , treemaxx]
// const headIX = 0; 
//     const strategyIX = 1; 
//     const childsIX = 2; 
//     const treeMassIX = 3;
function updateSubtreeChild(idParent, idChild) 
{
    const subtree = subtreeList[idParent].
    subtree[childsIX].push(idChild); // müsste eigentlich eine Referenz sein
    //subtreeList[idParent] = subtree;
} 

function updateSubtreeStrgy(id, strategy, mass) 
{

    const subtree = subtreeList[id];

    subtree[strategyIX] = strategy; 
    subtree[treeMaIX] = mass; 

    console.log("Subtree now", id, subtree);
} 


function addSubtree(level, subtree, id)
{
    console.log("Addasubtrrr", id);
    subtreeList[id] = subtree; 
    if (!Array.isArray(layerList[level]))
    {
        layerList[level] = []; 
    }
    layerList[level].push(subtree);
}

function kFactor(level) {

    const term = -3*level*layerheight;
    return 20/term; 
}

// type list is of format [[number, type], [number, type]....]

// das gsn Element hat Masse, geschwindigkeit, Auftriebskonstante
// body ist die Liste aus [masse, kfactor, v]

function gsnElemGenerator(elemType, id, body, explanation) 
{
//function gsnElemGenerator(ElemType, name, TypeList) {
    console.log("Elem Generator ", elemType, id, body, explanation);
    // x,Y muss noch auf default gesetzt werden
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

    switch(elemType)
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
    elemCont.name = elemType; 
    gsnElement.anchor.set(0.5);
    elemCont.id = id; 
    elemCont 
            .on('pointerdown', onDragStart)
            .on('pointerup', onDragEnd)
            .on('pointerupoutside', onDragEnd)
            .on('pointermove', onDragMove);

    elemCont.x = canvasWidth/2; 
    elemCont.y = canvasHeight/2; 

    const shiftX = gsnElement.width/2; 
    const shiftY = gsnElement.height/2; 

    var pixiObjects = explanation.map(symbolGenerator);
    var layout = calcLayout(gsnElement);


    for (var i = 0; i < 3; i++)
    {
        const number = explanation[i][0];
        const typeID = explanation[i][1];

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

    // embodyment
    elemCont.mass = body[iMass];
    elemCont.k = body[iKFact];
    elemCont.v = body[iV];

    // Linie hinzufügen
    const line = new PIXI.Graphics();
        // jetzt noch die Linie
    line.lineStyle(5, 0x5F5F5A, 10);
    line.moveTo(elemCont.x, elemCont.y);
    line.lineTo(elemCont.x, elemCont.y - 5);
    line.zIndex = -id;
    playWindow.vpRef.addChild(line); // container global
   
    elemCont.incomming = line;
    playWindow.vpRef.addChild(elemCont);
    console.log("Zindex", elemCont.zIndex);
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

    console.log("Type: ", typeID);
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

