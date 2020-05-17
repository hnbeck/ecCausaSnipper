
// subtree = body
// [goal, strategy, [subtrees]]
// das müssen die Elemente der Layer sein
// goal ist der Kopf des Subtrees

// layer is a list of subtrees, each subtree has a body
// layerlist = [subtree1, subtree2....], with index = layer no
var layerList = []; 
var solutionList = []; 
// subtreeList is a list of all subtrees, 
// describing the embodyment of a subtree
// subtree = [goal, strategy, [Subtrees], mass], this are the PIXI objects
// index in subtreeList = goal.id; 
var subtreeList = []; // is in old code the body[]

var layerheight = 100; //default 
var numberStrategy = 1; 
// einige wichtige Indizes
const iMass = 0; 
const iKFact = 1; 
const iV = 2; 
var flipSign = -1;

// subtree kommt von Prolog als Liste, fast wie der fertige Body
// [['goal', ID, body, explanation], ['strategy', ID, Body, explanation], [...] , treemaxx]
// const headIX = 0; 
//     const strategyIX = 1; 
//     const childsIX = 2; 
//     const treeMassIX = 3;
//      const parendIX = 4; 
function updateSubtreeChild(idParent, idChild, childmass) 
{
    console.log("Subtrees at the moment ", subtreeList);

    const subtree = subtreeList[idParent];
    subtree[treeMassIX] += childmass; 
    subtree[childsIX].push(idChild); 

    // lagekorrektur
    const childTree = subtreeList[idChild];
    childTree[headIX].x = subtree[headIX].x  + childTree[headIX].v*50; 

    console.log("Subtree goal now", subtree);
} 

function updateSubtreeStrategy(id, strategy, mass) 
{
    console.log("Subtrees at the moment ", subtreeList);

    const subtree = subtreeList[id];
    const parentID = subtree[parentIX];

    subtree[strategyIX] = strategy; 
    subtree[treeMassIX] = mass; 

    propagateMass(parentID, subtree[treeMassIX]);
    console.log("Subtree strategy now", id, subtree);
} 

function updateSubtreeSolution(id, solution, mass) 
{
    console.log("Subtrees before solution at the moment ", subtreeList);

    const subtree = subtreeList[id];
    const parentID = subtree[parentIX];

    solution.v = 0; 
    subtree[strategyIX] = solution; // strategiesy and solution have the same place in subtree
    subtree[treeMassIX] = mass; 

    propagateMass(parentID, subtree[treeMassIX]);
    console.log("Subtree solution now", id, subtree);
} 

function propagateMass(parentID, mass)
{   
    if (parentID != "root")
    {
        console.log("Parent is ", parentID);
        const subtree = subtreeList[parentID];
        subtree[treeMassIX] += mass; 
        const id = subtree[parentIX];
        propagateMass(id, subtree[treeMassIX]);
    }
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

// explanation list is of format [[number, type], [number, type],[number, type]]
// transform list is of same format, but the first is the header, der rest the body

// das gsn Element hat Masse, geschwindigkeit, Auftriebskonstante
// body ist die Liste aus [masse, kfactor, v]

function gsnElemGenerator(elemType, id, body, explanation) 
{
//function gsnElemGenerator(ElemType, name, TypeList) {
    console.log("Elem Generator ", elemType, id, body, explanation);
    // x,Y muss noch auf default gesetzt werden
   
    const elemCont = new PIXI.Container();
    var parentContainer =  playWindow.vpRef;
    var lineAlpha = 1.0; 
    var midSymbol; 
    var ressourceID;
    var symbolAlpha = 1.0; 
    

    // embodyment
    elemCont.id = id; 
    elemCont.name = elemType; 
    elemCont.interactive = true; 
    elemCont.mass = body[iMass];
    elemCont.k = body[iKFact];
    elemCont.v = body[iV];
    // for interaction
    elemCont.touched = false; 
    elemCont.dragging = false; 

    elemCont 
            .on('pointerdown', onDragStart)
            .on('pointerup', onDragEnd)
            .on('pointerupoutside', onDragEnd)
            .on('pointermove', onDragMove);
    
    switch(elemType)
    {
        case ('strategy'): 
            ressourceID = "/graphics/strategy.png"; 
            explanation.push([-1, 'ar']);
           
            // strategies residing in the ressource bar have no mass
            if (elemCont.mass == 0) 
            {   
                parentContainer = ressourceWindow.vpRef; 
                elemCont.x = ressourceWindow.width/2-10;
                elemCont.y = numberStrategy*layerheight;
                numberStrategy++;
                lineAlpha = 0.0; 
            } 
            else
            {
                elemCont.x = 2000; 
                elemCont.y = canvasHeight/2;
            }

        break;

        case ('solution'):
            elemCont.x = viewportSize/2 + Math.random()*100*flipSign; 
            elemCont.y = canvasHeight/2;
            flipSign *= -1; 
            ressourceID = "/graphics/solution.png"; 
            midSymbol = null; 
            symbolAlpha = 0.8;
            // in this case its a ressource solution (like mass = 0 for strategy)
            if (elemCont.v > 0) 
                solutionList.push(elemCont);
        break

        case ('goal') :
            const dx = elemCont.v; 
            elemCont.x = viewportSize/2 + dx; 
            elemCont.y = canvasHeight/2;
            ressourceID = "/graphics/goal.png"; 
            midSymbol = null; 
            
        break; 
    }
   
    const gsnElement = new PIXI.Sprite(
                PIXI.loader.resources[ressourceID].texture
            );  
    gsnElement.anchor.set(0.5);
    gsnElement.scale.set(symbolAlpha);
    elemCont.addChild(gsnElement);
    
    lettering(elemCont, gsnElement, explanation);
 
    // Linie hinzufügen
    const line = new PIXI.Graphics();
        // jetzt noch die Linie
    line.lineStyle(5, 0x5F5F5A, 10);
    line.moveTo(elemCont.x, elemCont.y);
    line.lineTo(elemCont.x, elemCont.y-1);
    line.zIndex = -id;
    line.alpha = lineAlpha; 
    playWindow.vpRef.addChild(line); // container global
   
    elemCont.incomming = line;
    parentContainer.addChild(elemCont);

    return elemCont;
}

function lettering(elemCont, gsnElement, explanation)
{
     const textMargin = 5;

     const style = new PIXI.TextStyle({
        fontFamily: 'Arial',
        fontSize: 14,
        fill: '#FFFFFF'
    });

    const shiftX = gsnElement.width/2; 
    const shiftY = gsnElement.height/2; 

    var pixiObjects = explanation.map(symbolGenerator);
    var layout = calcLayout(gsnElement);

    for (var i = 0; i < explanation.length; i++)
    {
        const number = explanation[i][0];
        //const typeID = explanation[i][1];
        const element = pixiObjects[i];
        
        const basicText = new PIXI.Text(number.toString(), style);
        basicText.anchor.y = 1.0; 
        basicText.x = layout[elemCont.name][i][0] - shiftX;
        basicText.y = layout[elemCont.name][i][1] - shiftY;
        
        element.x = basicText.x + basicText.width + textMargin;
        element.y = basicText.y;

        if (number != -1) 
        {
            elemCont.addChild(basicText);
        }
       
        elemCont.addChild(element);
    
        // else
        // {
        //     const midSymbol = pixiObjects[3];

        //     midSymbol.x = layout['arw'][0]-shiftX + correction + 6;
        //     midSymbol.y = layout['arw'][1]-shiftY;
        //     elemCont.addChild(midSymbol);
        // }
       
    }
}

function calcLayout(element) 
{
    var layout;

    const w = element.width; 
    const h = element.height; 
    const margin = element.height / 4;

    const midX = w/2; 
    const midY = h/2;

    var layout = {  goal: { 0: [margin, h - margin], 
                            1: [midX, h - margin],
                            2: [midX, midY]},
                    strategy: { 0: [margin+5, h - margin], 
                                1: [midX+5, h - margin],
                                2: [midX+5, midY],
                                3: [margin + midX/2 - 6, h - margin]},
                    solution: {  0: [midX-midX/3, midY+midY/4]}
                };
                   

    return layout; 
}
// generates a pixi object according the type
// elem is of format [number, type]
function symbolGenerator(argument) {

    const typeID = argument[1];
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
            break; 
        case 'ar':
            type = new PIXI.Sprite(
                PIXI.loader.resources["/graphics/arrow.png"].texture
            );
            type.anchor.x = 0.5; 
            type.scale.x = 0.2;
            type.scale.y = 0.3;
    }   
    type.anchor.y = 1.0; 

    return type;
}

