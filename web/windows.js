///////////////////////////////////////////////////////////////////////////////////////////////////
// 
//  window.js realizes a window with background, scaling and drag drop
//
//  (c) Hans N Beck
//
//////////////////////////////////////////////////////////////////////////////////////////////////

function windowGenerator(winConf) 
{
    const window = new PIXI.Container(); 
    const marker = new PIXI.Graphics();
    const area = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/windowarea.png"].texture
    );
   
    ///// create viewport /////
    // indication of viewpoint area which may be much larger than camera view
    area.alpha = winConf.alpha;
    area.name = "area";
    area.width = winConf.width;
    area.height = winConf.height;

    const viewport = viewportGenerator(winConf);
          
    marker.lineStyle(6, 0x282200, 0.2);
    marker.drawCircle(winConf.width/2, winConf.height/2, 10);
    marker.drawRect(0,0,winConf.width, winConf.height);
    viewport.addChild(area, marker);
    
    // create layer for augmentation of buttons
    const augmentLayer = augmentGenerator(winConf, viewport);
    
     // show only what camera can see
    viewport.mask = cameraGenerator(winConf.winWidth, winConf.winHeight);
    augmentLayer.addChild(viewport.mask);
    augmentLayer.name = "augment"; 
    window.vpRef = viewport; 
    window.name = 'window'+ winConf.name;
    window.addChild(viewport);
    window.addChild(augmentLayer);

    return window; 
}

function viewportGenerator(winConf)
{
    const viewport = new PIXI.Container();

    viewport.scale.set(winConf.scale);
    viewport.position = viewportPos(winConf);
    viewport.pivot.set(0.5);
    viewport.interactive = true;
    viewport.buttonMode = false; 
    viewport.filter = {x: winConf.x, y: winConf.y};

    viewport.name = winConf.name;
    viewport.margin = winConf.margin;
    viewport.leave =  winConf.leave; 
    viewport.enter =  winConf.enter; 
    viewport
            .on('pointerdown', onPointerStart)
            .on('pointerup', onPointerEnd)
            .on('pointerupoutside', onPointerEnd)
            .on('pointermove', onPointerMove);

    return viewport;
}
// position the viewport in window
// the main viewport shall be positioned such that the mid is in the mid of the window
function viewportPos(winConf)
{
    const pos = new PIXI.Point(0,0);
   
    if (winConf.name == "play")
    {
        pos.x =  -winConf.width/2*winConf.scale + winConf.winWidth/2;
        //pos.y = winConf.winHeight/10;
    }
    return pos; 
}

// camera for mask
function cameraGenerator(w,h) 
{
    const camera = new PIXI.Graphics();

    camera.beginFill(0xFFFFFF,0.5);
    camera.drawRect(0,0, w, h);
    camera.endFill();

    return camera; 
}

function augmentGenerator(winConf, viewport){

    const augmentLayer = new PIXI.Container();
    const btUp = 0; 
    const btDown = 1; 

    if (winConf.type == 's')
    {
        const buttons = buttonGenerator(); 

        buttons[btUp].x = winConf.winWidth - (buttons[btUp].width)*1.9; 
        buttons[btUp].y = buttons[btUp].height*0.8; 
        buttons[btDown].x = winConf.winWidth - (buttons[btDown].width)*0.8;     
        buttons[btDown].y = buttons[btDown].height*0.8; 
        augmentLayer.addChild(buttons[btUp]);
        augmentLayer.addChild(buttons[btDown]);
        viewport.scaling = true; 
    }
    else
        viewport.scaling = false; 
    
    augmentLayer.x = 0; 
    augmentLayer.y = 0; 
    augmentLayer.vpRef = viewport; 

    return augmentLayer; 
}

function buttonGenerator () {

    const btUp = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/bt_scaleup.png"].texture
    );
    const btDown = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/bt_scaledown.png"].texture
    );

    btUp.interactive = true;
    btUp.buttonMode = true; 
    btUp.alpha = 0.7;
    btUp.anchor.set(0.5);
    btDown.interactive = true; 
    btDown.alpha = 0.7;
    btDown.anchor.set(0.5);

    btUp
        .on('pointerdown', onBtUpDown)
        .on('pointerup', onBtUpUp)
        .on('pointerupoutside', onBtUpUp)
        .on('pointerover', onBtUpOver)
        .on('pointerout', onBtUpOut);

    btDown
        .on('pointerdown', onBtDownDown)
        .on('pointerup', onBtDownUp)
        .on('pointerupoutside', onBtDownUp)
        .on('pointerover', onBtDownOver)
        .on('pointerout', onBtDownOut);

   return [btUp, btDown];

}


/////////////////////////////// Configurations ////////////////////////////////////////////

function configMain() 
{
    const leaveFunc = function (aPixiObject) 
        {
            //is.removeChild(aPixiObject); // this is the viewport which owns this function
            aPixiObject.x = 0; 
        }

    const enterFunc = function (aPixiObject, vpFrom) 
        {
            const origin = new PIXI.Point(0,0);
            const newPos = this.toLocal(origin, aPixiObject);
           
            this.addChild(aPixiObject); 
            aPixiObject.borderCrossX = newPos.x; 
            aPixiObject.borderCrossY = newPos.y;
            vpFrom.removeChild(aPixiObject);
        }

    const configSet = {
        x:1, 
        y:1, 
        scale: 1.0, 
        name: 'play',
        alpha: 0.2, 
        type: 's',
        width: viewportSize, 
        height: viewportSize, 
        winWidth: canvasWidth-ressourceSize-paletteSize,
        winHeight: canvasHeight,
        margin: 0,
        leave: leaveFunc,
        enter: enterFunc
    }

    return configSet;
}
 
function configRessource() 
{
    const leaveFunc = function (aPixiObject) 
        {
           // his.removeChild(aPixiObject); // this is the viewport which owns this function
            aPixiObject.x =0; 
        }

    const enterFunc = function (aPixiObject, vpFrom) 
        {
            const origin = new PIXI.Point(0,0);
            const newPos = this.toLocal(origin, aPixiObject);
            newPos.x -= aPixiObject.width; 
            this.addChild(aPixiObject); 
            aPixiObject.borderCrossX = ressourceSize/2; 
            aPixiObject.borderCrossY = newPos.y;
            vpFrom.removeChild(aPixiObject);
        }

    const configSet = {
        x:0, 
        y:1, 
        scale: 1.0, 
        name: 'ressource',
        alpha: 0.4, 
        type: 'r',
        width: ressourceSize, 
        height: 2000, 
        winWidth: ressourceSize,
        winHeight: canvasHeight,
        margin: 20,
        leave: leaveFunc,
        enter: enterFunc
    }

    return configSet; 
}

function configPalette() 
{
     const leaveFunc = function (aPixiObject)
        {
            aPixiObject.x = 0; 
        }

    const enterFunc = function(aPixiObject, vpFrom) 
        {
            // this is here the viewport entered
            const origin = new PIXI.Point(0,0);
            const newPos = this.toLocal(origin, aPixiObject);
            this.addChild(aPixiObject); 
            aPixiObject.borderCrossX = paletteSize/2; 
            aPixiObject.borderCrossY = newPos.y;
            vpFrom.removeChild(aPixiObject);
        }

     const configSet = {
        x:0, 
        y:1, 
        scale: 1.0, 
        name: 'palette',
        alpha: 0.5, 
        type: 'r',
        width: paletteSize, 
        height: 2000, 
        winWidth: paletteSize,
        winHeight: canvasHeight,
        margin:20,
        leave: leaveFunc,
        enter: enterFunc
    }
    return configSet; 
}

////////////////////// scaling / dragging call backs for touch /////////////////////////

function onPointerStart(event) 
{
    // store a reference to the data
    // the reason for this is because of multitouch
    // we want to track the movement of this particular touch

    const px = 0;
    const py = 1; 
    const pID = 2; 

    // save for later use
    this.data = event.data;
    if (!this.dragging) 
    {
        this.touchpoints = []; 
        this.dragging = true;
    }
    this.touchpoints.push([this.data.global.x, this.data.global.y, this.data.identifier]);
    // scale
    this.lastdistance = 0; 
    prio = true;
}

function onPointerEnd() 
{
    this.dragging = false;
    // set the interaction data to null
    this.data = null;
    this.touchpoints = null; 
    this.lastdistance = 0; 
    prio = false; 
}

function onPointerMove(event) 
{
    const px = 0;
    const py = 1; 
    const pID = 2; 
    const filterX = this.filter.x;
    const filterY = this.filter.y; 


    if (this.dragging) 
    {
        // mulittouch or not?
        if (this.touchpoints.length < 2)
        {
            // only one point in list, ths last touchpoint
            this.x -= (this.touchpoints[0][px] - this.data.global.x) * filterX;
            this.y -= (this.touchpoints[0][py] - this.data.global.y) * filterY;
            this.touchpoints[0][px] = this.data.global.x; 
            this.touchpoints[0][py] = this.data.global.y; 
        }
        else
        {
            if (this.scaling) 
            {
                const oldScaleX = this.scale.x; 
                const oldScaleY = this.scale.y; 

                const index = this.touchpoints.findIndex(selectPoint, this.data);
                this.touchpoints[index][px] = this.data.global.x; 
                this.touchpoints[index][py] = this.data.global.y;
               
                const deltax = Math.abs(this.touchpoints[0][px]-this.touchpoints[1][px]);
                const deltay = Math.abs(this.touchpoints[0][py]-this.touchpoints[1][py]);

                const distance = (deltax + deltay)/this.scale.x;
                
                if (this.lastdistance == 0)
                {
                    this.lastdistance = distance; 
                }

                const scaleDelta = (distance/this.lastdistance)-1; // distance to 1.0
               
                this.scale.x += this.scale.x*scaleDelta;
                this.scale.y += this.scale.y*scaleDelta;
                //console.log("Scale", this.lastdistance, distance, scaleDelta,  this.scale.x);

                // Positionskorretur
                // startposition des viewport in x war  -viewportSize/2 * scale + winWidth/2
                this.x = this.x + viewportSize/2 * (oldScaleX -this.scale.x); 
                //this.y = this.y +  (oldScaleY - this.scale.y); 
            }
        }
    }
}

// help function for array iterator
function selectPoint(value, index, array)
{
    const px = 0;
    const py = 1; 
    const pID = 2; 
   
    return value[pID] == this.identifier;  
}


//////////////////////////////////// button callbacks //////////////////////////////

function onBtUpDown () 
{
    const vp = this.parent.vpRef; 
    const oldScaleX = vp.scale.x;
    vp.scale.x += 0.05;
    vp.scale.y += 0.05;
    vp.x = vp.x + viewportSize/2 * (oldScaleX -vp.scale.x); 
    this.scale.set(1.2);
}

function onBtUpOver () 
{
    this.alpha = 1.0;
}

function onBtUpUp() 
{
    this.scale.set(1.0);
    this.alpha = 0.7;
    this.scale.set(1.0);
}

function onBtUpOut () 
{

}

function onBtDownDown () 
{
    const vp = this.parent.vpRef;
    const oldScaleX = vp.scale.x;
    vp.scale.x -= 0.05;
    vp.scale.y -= 0.05;
    vp.x = vp.x + viewportSize/2 * (oldScaleX -vp.scale.x); 
    this.scale.set(1.2);
    console.log("bummer");
}

function onBtDownOver () 
{
    this.alpha = 1.0;
}

function onBtDownOut () 
{
    
}


function onBtDownUp() {

    this.scale.set(1.0);
    this.alpha = 0.7;
    this.scale.set(1.0);
}