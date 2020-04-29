
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


// subtree kommt von Prolog als Liste, fast wie der fertige Body
// [['goal', ID, body, explanation], ['strategy', ID, Body, explanation], [...] , treemaxx]
// const headIX = 0; 
//     const strategyIX = 1; 
//     const childsIX = 2; 
//     const treeMassIX = 3;
function updateSubtreeChild(idParent, idChild, childmass) 
{
    const subtree = subtreeList[idParent];
    subtree[treeMassIX] += childmass; 
    subtree[childsIX].push(idChild); 

    console.log("Subtree goal now", subtree);
} 

function updateSubtreeStrategy(id, strategy, mass) 
{
    const subtree = subtreeList[id];

    subtree[strategyIX] = strategy; 
    subtree[treeMassIX] = mass; 

    console.log("Subtree strategy now", id, subtree);
} 

// adds a subtree to the subtreelist, index is the id of the head goal
function addSubtree(level, subtree, id)
{
    subtreeList[id] = subtree; 
    if (!Array.isArray(layerList[level]))
    {
        layerList[level] = []; 
    }
    layerList[level].push(subtree);

    console.log("Layers now ", layerList);
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

    const dx = elemCont.v*10; 
    elemCont.x = canvasWidth/2 + dx; 
    elemCont.y = canvasHeight/2; 

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

