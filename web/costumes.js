
//////// Basic lists for rendering
// list of elements in a layer
var layerList = []; 
// list of solutions
var solutionList = []; 
// subtreeList is a list of all subtrees, 
// describing the embodyment of a subtree
// subtree = [goal, strategy, [Subtrees], mass], this are the PIXI objects
// index in subtreeList = goal.id; 
var subtreeList = []; // is in old code the body[]
var subtreeTemplate = { 'goal': 0, 
                        'strategy' : 0,
                        'childs' : 0, 
                        'parent' : 0,
                        'mass' : 0,
                        'interval' : []
                        }

const iMass = 0;
const iKFact = 1; 

var numberStrategy = 1; 
var numberSolution = 1; 

// ?(S,A)
// !(S, S*)
// +(S, C) Struktur zu cause
// *(C, E*) cause to effect
// >(E, S, S*) 

////// interface to PROLOG: operations +(S,C) //////////////////////

////// a child is always itself a subtrees
function updateSubtreeChild(idParent, idChild, childmass) 
{
    console.log("CHILD: Subtrees at the moment ", subtreeList);

    const subtree = subtreeList[idParent];
    subtree.mass += childmass; 
    subtree.childs.push(idChild); 

    // lagekorrektur
    const childTree = subtreeList[idChild];
  
    console.log("CHILD Subtree goal now", subtree);
} 

// put a strategy into the subtree given by id
function updateSubtreeStrategy(id, strategy, mass) 
{
    console.log("STRATEGY: Subtrees at the moment + id + mass ", subtreeList, id, mass);
    const subtree = subtreeList[id];
  
    subtree.strategy = strategy; 
    subtree.mass = mass; 

    // If strategy added all "free " solutions has to increase the level
    for (var i = 0; i < solutionList.length; i++)
    {
        const fact = 1/ solutionList[i].k;
        solutionList[i].k = 1/(fact - (6*layerheight/20));
    }
    console.log("STRATEGY Subtree strategy now", id, subtree);
    console.log("STRATEGY: SubtreeList at the moment ", subtreeList);
} 

// put a solution into the subtree given by id
function updateSubtreeSolution(id, solution, mass) 
{
    console.log("SOLUTION Subtrees at the moment ", subtreeList);
    const subtree = subtreeList[id];
 
    solution.v = 0; 
    subtree.strategy = solution; // strategiesy and solution have the same place in subtree
    subtree.mass = mass; 

    console.log("SOLUTION Subtree solution now", id, subtree);
} 

function updateSubtreeEmbodiment(id, mass, interval)
{  
    const subtree = subtreeList[id];
    subtree.interval = interval; 
    subtree.mass = mass; 
}

// adds a subtree to the subtreelist, index is the id of the head goal
// in general, in the first step, the subtree contains only the goals
function addSubtree(level, subtree, id)
{
   //console.log("ADD ANGEKOMMEN", subtree);
   // converstion to numeric is needed
   const iv = subtree[5]; 

    newSubtree = { 'goal' : subtree[headIX],
                   'strategy' : subtree[strategyIX],
                    'childs' : subtree[childsIX],
                    'mass' : subtree[treeMassIX], 
                    'parentID' : subtree[parentIX],
                    'level': level,
                    'interval' : iv};

    subtreeList[id] = newSubtree; 
    //console.log("NEW SUBTREE", newSubtree);

    if (!Array.isArray(layerList[level]))
    {
        // create an empty subgraph, every node represents a cell of width
        layerList[level] = [];
    } 

    layerList[level].push(newSubtree);
    console.log("ADD: Layers now ", layerList);
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
    // for interaction
    elemCont.touched = false; 
    elemCont.dragging = false; 
    elemCont.linked = false; 
    elemCont.autoMove = moveNeutralFunc; 

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
                elemCont.x = ressourceWindow.width/2;
                elemCont.y = numberStrategy*layerheight;
                numberStrategy++;
                lineAlpha = 0.0; 
                elemCont.mass = 70; 
            } 
            else
            {
                elemCont.x = 2000; 
                elemCont.y = canvasHeight/2;
            }

            break;

        case ('solution'):

            ressourceID = "/graphics/solution.png"; 
            if (elemCont.mass == 0) 
            {   
                parentContainer = paletteWindow.vpRef; 
                elemCont.x = paletteWindow.width/2;
                elemCont.y = numberSolution*layerheight;
                numberSolution++;
                lineAlpha = 0.0; 
                symbolAlpha = 0.7; 
                elemCont.mass = 70; 
            } 
            else
            {
                elemCont.x = 2000; 
                elemCont.y = canvasHeight/2;
            }

            break;

        case ('goal') :
            const dx = elemCont.v; 

            elemCont.x = viewportSize/2 + dx; 
            elemCont.y = canvasHeight/2;
            ressourceID = "/graphics/goal.png"; 
            midSymbol = null; 
            elemCont.receptor = true; 

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
    const margin = element.height / 5;

    const midX = w/2; 
    const midY = h/2;

    var layout = {  goal: { 0: [margin, h - margin], 
                            1: [midX, h - margin],
                            2: [midX, midY]},
                    strategy: { 0: [margin+5, h - margin], 
                                1: [midX+7, h - margin],
                                2: [midX+5, midY],
                                3: [midX/2 + margin, h - margin]},
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
            type.scale.x = 0.15;
            type.scale.y = 0.4;
    }   
    type.anchor.y = 1.0; 

    return type;
}

