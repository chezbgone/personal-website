var metadata = { title: "Unwillingness", author: "Wayne Zhao", date: "2020",
instructions: "Click below or add your own parameters to the URL to change the “spin” and to create variation in the narrative discourse. You have to use the official names of “actors” for narrator and narratee, which can be found by looking at the code. Examples:",
examples: [
"narrator=huashi,speaking=after",
"narratee=jayden,order=retrograde",
"narrator=srini,narratee=ashley,speaking=before,order=random,time_markers,event_numbers,expression_numbers",
] };

// PLACES first
place.dorm = new Place("a", "dorm");

// ACTORS next
actor.liam = new Actor(null, "Liam", spatial.in, place.dorm, pronoun.masculine);
actor.srini = new Actor(null, "Srini", spatial.in, place.dorm, pronoun.masculine);
actor.ashley = new Actor(null, "Ashley", spatial.in, place.dorm, pronoun.feminine);
actor.jayden = new Actor(null, "Jayden", spatial.in, place.dorm, pronoun.masculine);
actor.huashi = new Actor(null, "Huashi", spatial.in, place.dorm, pronoun.feminine);

// THINGS next
thing.college = new Thing("an", "college", spatial.of, actor.liam);
thing.college.owner = actor.liam;
thing.game = new Thing("an", "e-sports game", spatial.of, thing.college);
thing.game.owner = thing.college;
thing.friends = new Thing("the", "friends", spatial.of, actor.liam);
thing.friends.owner = actor.liam;
thing.team = new Thing("the", "team", spatial.of, actor.huashi);
thing.team.owner = actor.huashi
thing.unknown_team = new Thing("some", "completely unknown team", spatial.of, actor.cosmos)

// Finally, EVENTS
var EXIST = new Event(actor.liam, "is", null, temporal.in, place.dorm);
var LEARN_ABOUT_GAME = new Event(actor.liam, "hear about", thing.game);
var PLAN_FOR_GAME = new Event(actor.liam, "plan to attend", thing.game);
var DECIDE_TO_ASK = new Event(actor.liam, "decide to ask", [actor.srini, actor.ashley], temporal.to, "compete together", "quickly");
var FRIENDS = new Event([actor.srini, actor.ashley], "is", null, null, thing.friends);
var SUMMON_FRIENDS = new Event(actor.liam, "get", [actor.srini, actor.ashley], temporal.to, "form a team", "with some difficulty");
var CHALLENGERS_APPEAR = new Event([actor.jayden, actor.huashi], "is", null, null, "some of the school's best gamers", "at that time");
var THE_INVITATION = new Event(actor.huashi, "invite", actor.liam, temporal.to, thing.team);
var INDECISION = new Event(actor.liam, "equivocate");
var ASK = new Event(actor.liam, "ask", thing.friends, null, "what to do");
var SRINI_RESPONSE = new Event(actor.srini, "say", '"Moving to the other team is a bit of a jerk move"', temporal.to, actor.liam);
var ASHLEY_RESPONSE = new Event(actor.ashley, "say", '"It\'s a really good offer which makes sense to consider, and which anyone might actually take up"', temporal.to, actor.liam, "rationalizing");
var MORE_INDECISION = new Event(actor.liam, "have trouble deciding", null, null, "what to do", "still");
var FINAL_DECISION = new Event(actor.liam, "decide to compete", null, temporal.with, thing.friends, "finally");
var REJECTION = new Event(actor.liam, "say", "no", temporal.to, actor.huashi);
var BEATING = new Event([actor.liam, thing.friends], "beat", [actor.jayden, actor.huashi], null, "one month later");
var LOSING = new Event(thing.unknown_team, "beat", [actor.liam, thing.friends]);

var world = new World(place, actor, thing, eventSeq);
function run() { narrate(metadata, {}, world); }
