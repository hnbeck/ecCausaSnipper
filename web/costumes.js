
// subtree = body
// [goal, strategy, [subtrees]]
// das müssen die Elemente der Layer sein
// goal ist der Kopf des Subtrees

// [[id, attribute, layer], [id, attribure, layer], [subtrees]]
// räumliche ausdehnnung der elemente
// körper, position
// parent is PIxi Container
// processierter Subtree
// [parent, goal, strategy, [List]] wobei goal Strategy Pixiobjekte sind
function embodySubtree(body, x,y, container, subtree){

    const goal = subtree[0];
    const strategy = subtree[1];
    const subsubtree = subtree[2];

    const glName = goal[0];
    const glAttributes = goal[1];
    const glLayer = goal[2];
    var line; 

    const goalCont  = gsnElemGenerator('goal', glName, glAttributes);
    goalCont.x = x; 
    goalCont.y = y; 
    goalCont.level = glLayer; 
    goalCont.k = kFactor(glLayer, layerheight);
    goalCont.id = glName; 
   
  
    body[0] = goalCont; 
    body[1] = []; 
    body[2] = []; 
    body[3] = 0; 

    //container.addChild(goalCont);
    var strategyCont; 

    if (strategy.length > 0){
        const stName = strategy[0];
        const stAttributes = strategy[1];
        const stLayer = strategy[2];

        strategyCont  = gsnElemGenerator('strategy', stName, stAttributes);
        strategyCont.x = x; 
        strategyCont.y = y + 5; 
        strategyCont.level = stLayer; 
       
        strategyCont.k = kFactor(stLayer, layerheight);
       
       
        line = new PIXI.Graphics();
        line.name = "e";
        // jetzt noch die Linie
        line.lineStyle(5, 0x5F5F5A, 10);
        line.moveTo(goalCont.x, goalCont.y);
        line.lineTo(strategyCont.x, strategyCont.y);
        container.addChild(line);
        strategyCont.incomming = line;

        body[1] = strategyCont; 
        container.addChild(strategyCont);
    }
    
      container.addChild(goalCont);

    var subtreeList = []; 
    var dx = [0, 10, -10, 15, -15, 20, -20, 25, -25, 30, -30];
    var v = [0, 1, -1, 2, -2, 3, -3, 4, -4, 5, -5];

    for (var i = 0; i < subsubtree.length; i++) {
        
         line = new PIXI.Graphics();
          container.addChild(line);
        //console.log(subsubtree);
        const subBody = embodySubtree([], x+dx[i], y + 20, container, subsubtree[i]);
        subBody[0].v = v[i];  //
        subtreeList[i] = subBody.slice();
        body[3] += subBody[0].mass;  
        
       
        line.name = "e";
        line.lineStyle(5, 0x5F5F5A, 10);
        line.moveTo(strategyCont.x, strategyCont.y);
        line.lineTo(subBody[0].x, subBody[0].y);
       
        subBody[0].incomming = line; 
    }

    if (body[3] == 0) {
        body[3] = goalCont.mass; 
    } else
    {
        body[3] *= 0.9; 
    }

    body[2] = subtreeList; 
    bodylist[goalCont.id]  = body; 
   
    add2LayerList(glLayer, body);

    return body; 
   

    
    

}

function bodyChilds(id) {

    const body = bodylist[id];
    if (body){
        return body[2];
    }
    else
    {
        return []; 
    }
}

function bodyMid(id) {

    const body = bodylist[id];
    return body[0].x; // x position des goals = kopf des bodys

}

function add2LayerList(layerNr, body)
{

    if (layerlist[layerNr] == null)
    {
        layerlist[layerNr] = []; 
    }
    layerlist[layerNr].push(body);
    
}

function kFactor(level, layerheight) {

    const term = -3*level*layerheight;
    return 20/term; 
}

// type list is of format [[number, type], [number, type]....]
function gsnElemGenerator(ElemType, name, TypeList) {

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
            elemCont.mass = 1000; 
        break;

        case ('solution'):
             ressourceID = "/graphics/solution.png"; 
             elemCont.mass = 1000; 
        break

        case ('goal') :
             ressourceID = "/graphics/goal.png"; 
             elemCont.mass = 1000; 
        break; 

    }
   
    const gsnElement = new PIXI.Sprite(
                PIXI.loader.resources[ressourceID].texture
            );  
    elemCont.addChild(gsnElement);
    elemCont.name = ElemType; 
    gsnElement.anchor.set(0.5);
    gsnElement.name = name; 
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

