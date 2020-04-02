

function windowGenerator(stage, width, height, winWidth, winHeight) {

    const viewport = new PIXI.Container();
    const augmentLayer = new PIXI.Container();
    const graphics = new PIXI.Graphics();

    const area = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/windowarea.png"].texture
    );
    const btUp = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/bt_scaleup.png"].texture
    );
    const btDown = new PIXI.Sprite(
        PIXI.loader.resources["/graphics/bt_scaledown.png"].texture
    );

    area.alpha = 0.25;
    area.width = width; 
    area.height = height; 
    viewport.interactive = true;
    viewport.buttonMode = false; 
    viewport
            .on('pointerdown', onPointerStart)
            .on('pointerup', onPointerEnd)
            .on('pointerupoutside', onPointerEnd)
            .on('pointermove', onPointerMove);
          

    btUp.interactive = true;
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
        .on('pointerout', onBtUpUp);

    btDown
        .on('pointerdown', onBtDownDown)
        .on('pointerup', onBtDownUp)
        .on('pointerupoutside', onBtDownUp)
        .on('pointerover', onBtDownOver)
        .on('pointerout', onBtDownUp);

    viewport.addChild(area);

    graphics.lineStyle(2, 0xBABABA, 1);
    graphics.drawRect(0,0, width, height);
    graphics.lineStyle(2, 0xFFBD01, 1);
    graphics.drawCircle(width/2, height/2,10);
    viewport.addChild(graphics);
    //viewport.pivot.x = viewport.width/2; 
    //viewport.pivot.y = viewport.height/2; 
    viewport.x = 0; 
    viewport.y = 0; 

    btUp.x = winWidth - (btUp.width)*2.0; 
    btUp.y = btUp.height*0.8; 
    btDown.x = winWidth - (btDown.width)*0.8;     
    btDown.y = btUp.height*0.8; 

    augmentLayer.addChild(btUp);
    augmentLayer.addChild(btDown);
    augmentLayer.areaRef = viewport; 

    stage.addChild(viewport);
    stage.addChild(augmentLayer);

    return viewport; 
}

////////////////////// scaling / dragging call backs for touch /////////////////////////

function onPointerStart(event) {
    // store a reference to the data
    // the reason for this is because of multitouch
    // we want to track the movement of this particular touch

    const px = 0;
    const py = 1; 
    const pID = 2; 

    // save for later use
    this.data = event.data;

    if (!this.dragging) {

        this.touchpoints = []; 
        this.dragging = true;
    }
    this.touchpoints.push([this.data.global.x, this.data.global.y, this.data.identifier]);
    // scale
    this.lastdistance = 0; 
    prio = true; 
}

function onPointerEnd() {
    this.dragging = false;
    // set the interaction data to null
    this.data = null;
    this.touchpoints = null; 
    this.lastdistance = 0; 
    prio = false; 
}

function onPointerMove() {

    const px = 0;
    const py = 1; 
    const pID = 2; 

    if (this.dragging) {
        // mulittouch or not?
        if (this.touchpoints.length < 2)
        {
            // only one point in list, ths last touchpoint
            
            this.x -= (this.touchpoints[0][px] - this.data.global.x);
            this.y -= (this.touchpoints[0][py] - this.data.global.y);
            this.touchpoints[0][px] = this.data.global.x; 
            this.touchpoints[0][py] = this.data.global.y; 
        }
        else
        {
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


//////////////////////////////////// button callbacks

function onBtUpDown () {
    
    const area = this.parent.areaRef;
    area.scale.x += 0.05;
    area.scale.y += 0.05;
    this.scale.set(1.2);
}

function onBtUpOver () {
    this.alpha = 1.0;
}

function onBtUpUp() {

    this.scale.set(1.0);
    this.alpha = 0.7;
    this.scale.set(1.0);
}

function onBtDownDown () {
    
    const area = this.parent.areaRef;
    area.scale.x -= 0.05;
    area.scale.y -= 0.05;
    this.scale.set(1.2);
    //console.log("parent:" , area.scale.x, area.scale.y);
}

function onBtDownOver () {
    this.alpha = 1.0;
}

function onBtDownUp() {

    this.scale.set(1.0);
    this.alpha = 0.7;
    this.scale.set(1.0);
}