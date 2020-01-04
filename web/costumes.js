var config = {
    type: Phaser.AUTO,
    width: 800,
    height: 600,
    parent: 'playContainer',
    scene: {
        preload: preload,
        create: create,
        update: update
    }
};


var game = new Phaser.Game(config);
var solution; 
var strategy; 
var goal; 

function preload ()
{
	 this.load.image('solution', '/graphics/solution.png');
     this.load.image('strategy', '/graphics/strategy.png');
     this.load.image('goal', '/graphics/goal.png');
 }

function create ()
{
	solution = this.add.sprite(200, 100, 'solution');
	strategy = this.add.sprite(200, 300, 'strategy');
	goal = this.add.sprite(400, 400, 'goal');
	solution.setScale(0.3);
	strategy.setScale(0.3);
	goal.setScale(0.3);
}

function update ()
{
}