/**
	An improved version of func_door_secret which is hopefully:
	1. Less buggy
	2. Easier to configure
	3. More flexible
*/

float FUNC_DOOR_SECRET_DIRECTION_INWARDS = 0;
float FUNC_DOOR_SECRET_DIRECTION_OUTWARDS = 1;
float FUNC_DOOR_SECRET_DIRECTION_LEFT = 2;
float FUNC_DOOR_SECRET_DIRECTION_RIGHT = 3;
float FUNC_DOOR_SECRET_DIRECTION_UP = 4;
float FUNC_DOOR_SECRET_DIRECTION_DOWN = 5;

// Directions are one of the above values
.float move_1_dir;
.float move_2_dir;
.float move_1_len;
.float move_2_len;
.float move_wait;
.vector original_angles;

void() func_door_secret2_move1;
void() func_door_secret2_move2;
void() func_door_secret2_move3;
void() func_door_secret2_move4;
void() func_door_secret2_move5;
void() func_door_secret2_move6;
void() func_door_secret2_done;

float SECRET_OPEN_ONCE = 1;		// stays open
float SECRET_NO_SHOOT = 8;		// only opened by trigger
float SECRET_YES_SHOOT = 16;	// shootable even if targeted

vector (float dir, vector forward, vector up) func_door_secret2_calculate_move_vec =
{
	switch (dir) {
		default:
		case FUNC_DOOR_SECRET_DIRECTION_INWARDS:
			return -forward;
		case FUNC_DOOR_SECRET_DIRECTION_OUTWARDS:
			return forward;
		case FUNC_DOOR_SECRET_DIRECTION_LEFT:
			return forward >< up;
		case FUNC_DOOR_SECRET_DIRECTION_RIGHT:
			return -(forward >< up);
		case FUNC_DOOR_SECRET_DIRECTION_UP:
			return up;
		case FUNC_DOOR_SECRET_DIRECTION_DOWN:
			return -up;
	}
};

void () func_door_secret2_use =
{
	self.health = 10000;

	// exit if still moving around...
	if (self.origin != self.oldorigin)
		return;

	self.message = string_null;

	SUB_UseTargets();

	if (!(self.spawnflags & SECRET_NO_SHOOT))
	{
		self.th_pain = SUB_NullPain;
		self.takedamage = DAMAGE_NO;
	}
	self.velocity = '0 0 0';

	// Make a sound, wait a little...

	sound(self, CHAN_VOICE, self.noise1, 1, ATTN_NORM);
	self.nextthink = self.ltime + 0.1;

	SUB_CalcMove(self.dest1, self.speed, func_door_secret2_move1);
	sound(self, CHAN_VOICE, self.noise2, 1, ATTN_NORM);
};

void(entity attacker, float damage) func_door_secret2_pain = { func_door_secret2_use(); };

// Wait after first movement...
void () func_door_secret2_move1 =
{
	self.nextthink = self.ltime + self.move_wait;
	self.think = func_door_secret2_move2;
	sound(self, CHAN_VOICE, self.noise3, 1, ATTN_NORM);
};

// Start moving sideways w/sound...
void () func_door_secret2_move2 =
{
	sound(self, CHAN_VOICE, self.noise2, 1, ATTN_NORM);
	SUB_CalcMove(self.dest2, self.speed, func_door_secret2_move3);
};

// Wait here until time to go back...
void () func_door_secret2_move3 =
{
	sound(self, CHAN_VOICE, self.noise3, 1, ATTN_NORM);
	if (!(self.spawnflags & SECRET_OPEN_ONCE))
	{
		self.nextthink = self.ltime + self.wait;
		self.think = func_door_secret2_move4;
	}
};

void () func_door_secret2_move4 =
{
	sound(self, CHAN_VOICE, self.noise2, 1, ATTN_NORM);
	SUB_CalcMove(self.dest1, self.speed, func_door_secret2_move5);
};

void () func_door_secret2_move5 =
{
	self.nextthink = self.ltime + self.move_wait;
	self.think = func_door_secret2_move6;
	sound(self, CHAN_VOICE, self.noise3, 1, ATTN_NORM);
};

void () func_door_secret2_move6 =
{
	sound(self, CHAN_VOICE, self.noise2, 1, ATTN_NORM);
	SUB_CalcMove(self.oldorigin, self.speed, func_door_secret2_done);
};

void () func_door_secret2_done =
{
	if (!self.targetname || self.spawnflags & SECRET_YES_SHOOT)
	{
		self.health = 10000;
		self.takedamage = DAMAGE_YES;
		self.th_pain = func_door_secret2_pain;
	}
	sound(self, CHAN_VOICE, self.noise3, 1, ATTN_NORM);
};

void () func_door_secret2_blocked =
{
	if (time < self.attack_finished)
		return;
	self.attack_finished = time + 0.5;
	T_Damage (other, self, self, self.dmg);
};

void () func_door_secret2_touch =
{
	// from Copper -- dumptruck_ds
	if (!CheckValidTouch()) return;

	if (self.attack_finished > time)
		return;

	self.attack_finished = time + 2;

	if (self.message != "")
	{
		centerprint (other, self.message);
		sound (other, CHAN_BODY, "misc/talk.wav", 1, ATTN_NORM);
	}
};

// Based on SetMovedir
vector (vector angles) func_door_secret2_normalize_angles {
	if (angles == '0 -1 0') {
		return '0 0 1';
	}
	else if (angles == '0 -2 0') {
		return '0 0 -1';
	}

	return angles;
}

void () func_door_secret2 =
{
	if (SUB_Inhibit()) {
		return;
	}

	// Setup sounds
	// TODO: Make each individual sound configurable?

	switch (self.sounds) {
		case 1:
			self.noise1 = "doors/latch2.wav";
			self.noise2 = "doors/winch2.wav";
			self.noise3 = "doors/drclos4.wav";
			break;
		case 2:
			self.noise2 = "doors/airdoor1.wav";
			self.noise1 = "doors/airdoor2.wav";
			self.noise3 = "doors/airdoor2.wav";
			break;
		case 3:
		default:
			self.noise2 = "doors/basesec1.wav";
			self.noise1 = "doors/basesec2.wav";
			self.noise3 = "doors/basesec2.wav";
			break;
	}

	precache_sound(self.noise1);
	precache_sound(self.noise2);
	precache_sound(self.noise3);

	if (!self.dmg) {
		self.dmg = 2;
	}

	if (!self.move_wait) {
		self.move_wait = 0;
	}

	local vector normalized_angles = func_door_secret2_normalize_angles(self.angles);
	makevectors(normalized_angles);

	local vector dir_1 = func_door_secret2_calculate_move_vec(self.move_1_dir, v_forward, v_up);
	local vector dir_2 = func_door_secret2_calculate_move_vec(self.move_2_dir, v_forward, v_up);

	self.dest1 = self.origin + dir_1 * self.move_1_len;
	self.dest2 = self.dest1 + dir_2 * self.move_2_len;

	// Set up basic attributes
	self.oldorigin = self.origin;
	self.solid = SOLID_BSP;
	self.angles = '0 0 0';
	self.movetype = MOVETYPE_PUSH;
	self.classname = "door";
	self.touch = func_door_secret2_touch;
	self.blocked = func_door_secret2_blocked;
	self.speed = 50;
	self.use = func_door_secret2_use;
	setmodel(self, self.model);
	setorigin(self, self.origin);

	if (!self.targetname || self.spawnflags & SECRET_YES_SHOOT)
	{
		self.health = 10000;
		self.takedamage = DAMAGE_YES;
		self.th_pain = func_door_secret2_pain;
		self.th_die = func_door_secret2_use;
	}

	if (!self.wait) {
		self.wait = 5;
	}
};
