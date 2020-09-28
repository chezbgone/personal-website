// Curveship-js version 0.2.0
//  Copyright 2019 Nick Montfort
//
// Copying and distribution of this file, with or without modification,
// are permitted in any medium without royalty provided the copyright
// notice and this notice are preserved. This file is offered as-is,
// without any warranty.
//
// Curveship-js is a partial implementation of Curveship, released in 2011.
// This JavaScript (ES6) Curveship is not intended to ever have a parser or an
// *interactive* world simulation. Narrative variation, the main point of the
// original system, is however enabled by this system.
//
// Version 0.2 is a "Micro Curveship" does not even fully implement the
// narrative variation of Curveship-py; for instance focalization and many
// changes in order for flashback and flashforward are not in place. If
// development continues, progress will be toward Curveship-js 0.6, which
// will have all the narrative variation capabilities (but no interactive
// simulation or parser capabilities) of Curveship-py 0.6.
//
// Web pages need to source this file, verb.js, and the story file to function.
//
// Some documentation is at http://nickm.com/curveship/js

var clock = 0;

function choice(array) {
  return array[~~(Math.random() * array.length)];
}

function shuffle(array) {
  var i = array.length, r, swap;
  while (i > 0) {
    r = Math.floor(Math.random() * i);
    i = i - 1;
    swap = array[i];
    array[i] = array[r];
    array[r] = swap;
  }
  return array;
}

class Existent {
  constructor(article, name) {
    if (new.target === Existent) { throw new TypeError("Can't directly instantiate Existent"); }
    if (article) { this.article = article; }
    this.name = name;
  }
  getNounPhrase(role, spin, ev) {
  // If person and number have non-null values, and spin applies, returns a pronoun
    var phrase = "";
    if (spin.narrator === this) {
      switch (role) {
      case "subject": { phrase = this.pronoun.getSubject(1, this.number); break; }
      case "object": { phrase = this.pronoun.getObject(1, this.number); }
      }
    } else if (spin.narratee === this) {
      switch (role) {
      case "subject": { phrase = this.pronoun.getSubject(2, this.number); break; }
      case "object": { phrase = this.pronoun.getObject(2, this.number); }
      }
    } else if (this.owner) {
      phrase = this.owner.getPossessive(spin, ev) + " " + this.name;
    } else if (givens.has(this) && lastNarratedEvent.hasParticipant(this)) {
      switch (role) {
      case "subject": { phrase = this.pronoun.getSubject(3, this.number); break; }
      case "object": { phrase = this.pronoun.getObject(3, this.number); }
      }
    } else if (!this.article) {
      phrase = this.name;
    } else if (givens.has(this) &&
      ["a", "an", "one", "several", "some"].includes(this.article)) {
      phrase = "the " + this.name;
    } else {
      phrase = this.article + " " + this.name;
    }
    return phrase;
  }
  getSubject(spin, ev) { return this.getNounPhrase("subject", spin, ev); }
  getObject(spin, ev) { return this.getNounPhrase("object", spin, ev); }
  getPossessive(spin, ev) {
    if (spin.narrator === this) { return this.pronoun.getPossessive(1, this.number, ev); }
    if (spin.narratee === this) { return this.pronoun.getPossessive(2, this.number, ev); }
    if (givens.has(this)) {
      if (ev.agent == this ||
          (typeof lastNarratedEvent.hasParticipant(this))) {
        return this.pronoun.getPossessive(3, this.number, ev);
      }
    }
    switch (this.number) {
    case 1: { return this.getSubject(spin, ev) + "’s"; }
    case 2: { return this.getSubject(spin, ev) + "’"; }
    }
  }
  configuredAs(spatialRelation, parent) {
    this.spatial = spatialRelation;
    this.parent = parent;
  }
  setNumberBasedOnArticle() {
    if (this.article === "") {
      this.number = 1;
    } else if (singularArticles.includes(this.article)) {
      this.number = 1;
    } else if (pluralArticles.includes(this.article)) {
      this.number = 2;
    } else { // The article is "the" or something more unusual. Can't use
         // it to determine whether this is singular or plural.
      if (name[-1] === "s") { // This is a guess; Plurals need not end
        this.number = 2;  // in -s. Number should be set manually for
      }             // other plurals.
      else {
        this.number = 1;
      }
    }
  }
}

// ### PRONOUNS ###
// Need to be defined here so Actor can use them by default

class PronounSet {
  constructor(thirdPersonSingular) {
    this.pronoun = [];
    this.pronoun.push([ [], [], [] ]); // There is no 0th person or number
    this.pronoun.push([ [], ["I", "me", "my"], ["we", "us", "our"] ]);
    this.pronoun.push([ [], ["you", "you", "your"], ["you", "you", "your"] ]);
    this.pronoun.push([ [], thirdPersonSingular, ["they", "them", "their"] ]);
  }
  getSubject(person, number = 1) {
    return this.pronoun[person][number][0];
  }
  getObject(person, number = 1) {
    return this.pronoun[person][number][1];
  }
  getPossessive(person, number = 1) {
    return this.pronoun[person][number][2];
  }
}

var pronoun = {};
pronoun.feminine = new PronounSet(["she", "her", "her"]);
pronoun.masculine = new PronounSet(["he", "him", "his"]);
pronoun.neuter = new PronounSet(["it", "it", "its"]);
pronoun.unknownBinary = new PronounSet(["she or he", "her or him", "her or his"]);
pronoun.nonBinary = new PronounSet(["they", "them", "their"]);


class Actor extends Existent {
  constructor(article, name, spatialRelation, parent, pronounSet = pronoun.neuter, number = 1) {
    super(article, name);
    this.spatial = spatialRelation;
    this.parent = parent;
    this.pronoun = pronounSet;
    this.number = number;
  }
}

var actor = { cosmos: new Actor(null, "it") };

class Place extends Existent {
  constructor(article, name) {
    super(article, name);
    this.visiblePlaces = {};
    this.configuredAs(spatial.of, actor.cosmos);
    this.pronoun = pronoun.neuter;
    this.setNumberBasedOnArticle();
  }
  addView(place, text, visibility = 1) { // "visibility" does nothing now.
  // It is used in Curveship.py, which has a complex model of what things
  // can be seen from what places. There, 1 means things are fully visible.
    this.visiblePlaces[place] = { text: text, visibility: visibility };
  }
}

var place = {};

class Thing extends Existent {
  constructor(article, name, spatialRelation, parent, prominence = .5) {
  // "prominence" does nothing now. It is used in Curveship.py, which has a
  // complex model of what things can be seen from what places. There, .5
  // means a thing is of average prominence.
    super(article, name);
    this.spatial = spatialRelation;
    this.parent = parent;
    this.prominence = prominence;
    this.pronoun = pronoun.neuter;
    this.setNumberBasedOnArticle();
  }
}

var thing = {};

class Event {
  constructor(agent, actionString, object, temporalRelation, extra, manner) {
    this.agent = agent;
    this.negated = false;
    if (actionString.slice(0, 4) === "not ") {
      this.negated = true;
      actionString = actionString.slice(4);
    }
    this.continuous = false;
    if (actionString.slice(-5) === " +ing") {
      this.continuous = true;
      actionString = actionString.slice(0, -5);
    }
    this.action = actionString;
    if (object) { this.object = object; }
    if (temporalRelation) { this.temporal = temporalRelation; }
    if (extra) { this.extra = extra; }
    this.manner = manner;
    this.setTemplate();
    this.start = clock;
    clock += 10;
    this.duration = 5;
    this.sense = null;
    eventSeq.push(this);
  }
  setTemplate(custom = null) {
    var actionArray;
    if (custom != null ) {
      this.template = custom;
    } else {
      this.template = "";
      this.template += "[agent/s]";
      if (this.manner) { this.template += " " + this.manner; }
      actionArray = this.action.split(" ");
      this.template += " [" + actionArray[0] + "/v]";
      if (actionArray.length > 1) { this.template += " " + actionArray.slice(1).join(" "); }
      if (this.object) {
        if (typeof this.object === "string") { this.template += " " + this.object; }
        else { this.template += " [object/o]"; }
      }
      if (this.temporal) { this.template += " " + this.temporal; }
      if (this.extra) { this.template += " [extra/o]"; }
    }
  }
  setSense(modality) {
    this.sense = modality;
  }
  currentTemplatesIn(actorOrThing, spatialRelation, parent) {
    actorOrThing.spatial = spatialRelation;
    actorOrThing.parent = parent;
  }
  placeVerbPhrase(currentTemplate, spin, agent) {
    var person = 3, number, slotExp = /\[([a-z]+)\/v\]/,
      base = slotExp.exec(currentTemplate)[1],
      verb = new Verb(base), tenseER, phrase;
    if (spin.narrator === agent) { person = 1; }
    if (spin.narratee === agent) { person = 2; }
    switch(spin.speaking) {
    case "after": { tenseER = "past"; break; }
    case "during": { tenseER = "present"; break; }
    case "before": { tenseER = "future"; break; }
    }
    if (Array.isArray(agent)) {
      number = 2;
    } else {
      number = agent.number;
    }
    phrase = verb.conjugatedVP(person, number, tenseER, spin.referring, this);
    currentTemplate = currentTemplate.replace(slotExp, phrase);
    return currentTemplate;
  }
  fixOrthography(sentence) {
    if (!"'\"'’”.!?".includes(sentence.slice(-1))) {
      sentence += ".";
    }
    if ("\"'’”".includes(sentence.slice(-1))) {
      if (!".!?".includes(sentence.slice(-2,-1))) {
        sentence = sentence.slice(0,-1) + "." + sentence.slice(-1);
      }
    }
    return sentence.slice(0,1).toUpperCase() + sentence.slice(1);
  }
  hasParticipant(actor) {
    return ((this.agent === actor) || (this.object === actor) || (this.extra === actor));
  }
  changeState(ex, spatial_1, parent_1, spatial_2, parent_2) {
    // Does nothing now. Will later be used for minimal world simulation, so
    // that focalization can be implemented.
  }
  realize(spin, fix = true) {
    var currentTemplate = this.template, subjectExp, objectExp,
      possessiveExp, subjectNP, objectNP, possessivePhrase = "", oldSpeaking;
    // Realize the verb phrase ...
    currentTemplate = this.placeVerbPhrase(currentTemplate, spin, this.agent);
    // Realize the noun phrases ...
    for (var existent of ["agent", "object", "extra"]) {
      if (this[existent]) {
        if (this[existent] instanceof Event) {
          oldSpeaking = spin.speaking;
          spin.speaking = "after";
          subjectNP = objectNP =  "that " + this[existent].realize(spin, false);
          spin.speaking = oldSpeaking;
        } else if (typeof this[existent] === "string") {
          subjectNP = objectNP = this[existent];
        } else if (Array.isArray(this[existent])) { // Only 2 elements are supported for now!
          // FIXME doesn't get pronouns in the right order: "You and I"
          subjectNP = this[existent][0].getSubject(spin, this) + " and " + this[existent][1].getSubject(spin, this);
          objectNP = this[existent][0].getObject(spin, this) + " and " + this[existent][1].getObject(spin, this);
          possessivePhrase = this[existent][0].getSubject(spin, this) + " and " + this[existent][1].getPossessive(spin, this);
          givens.add(this[existent][0]);
          givens.add(this[existent][1]);
        } else {
          subjectNP = this[existent].getSubject(spin, this);
          objectNP = this[existent].getObject(spin, this);
          possessivePhrase = this[existent].getPossessive(spin, this);
          givens.add(this[existent]);
        }
      }
      subjectExp = new RegExp("\\[" + existent + "\\/s\\]", "g");
      objectExp = new RegExp("\\[" + existent + "\\/o\\]", "g");
      possessiveExp = new RegExp("\\[" + existent + "\\'s\\]", "g");
      currentTemplate = currentTemplate.replace(subjectExp, subjectNP);
      currentTemplate = currentTemplate.replace(objectExp, objectNP);
      currentTemplate = currentTemplate.replace(possessiveExp, possessivePhrase);
    }
    if (fix) { currentTemplate = this.fixOrthography(currentTemplate); }
    return currentTemplate;
  }
}

var eventSeq = [], lastNarratedEvent;

class World {
  constructor(places, actors, items, eventSequence) {
    this.place = places;
    this.actor = actors;
    this.item = items;
    this.event = eventSequence;
  }
}

function narrate(metadata, spin, world) {
  var element = document.getElementById("narrative"), div,
    h1 = document.createElement("h1"),
    h2 = document.createElement("h2"),
    h3 = document.createElement("h3"),
    instructions = document.createElement("div"),
    examples = document.createElement("ul"),
    hr = document.createElement("hr"),
    telling = [], sentence, fix,
    oldReferring, i = 0, exp = 0, leftPart;
  for (var i = 0 ; i < world.event.length ; i++) { telling.push(i); }
  spin = getParameters(world.actor);
  h1.innerHTML = metadata.title;
  element.appendChild(h1);
  h2.innerHTML = metadata.author;
  if (metadata.hasOwnProperty("date")) {
    h2.innerHTML = metadata.author + ", " + metadata.date;
  }
  element.appendChild(h2);
  h3.innerHTML = "Generated by Curveship-js";
  element.appendChild(h3);
  instructions.innerHTML = metadata.instructions;
  element.appendChild(instructions);
  for (args of metadata.examples) {
    leftPart = window.location.href.split('?')[0]
    examples.innerHTML += '<li><a href="' + leftPart + '?' + args + '">?' + args + '</li>';
  }
  element.appendChild(examples);
  element.appendChild(hr);
  if (spin.order === "retrograde") { telling.reverse(); }
  if (spin.order === "random") { shuffle(telling); }
  div = document.createElement("div");
  element.appendChild(div);
  for (var i of telling) {
    event = world.event[i];
    div = document.createElement("div");
    sentence = "";
    if (spin.expression_numbers) {
      sentence += "<b>Exp " + exp + ":</b> ";
    }
    exp = exp + 1;
    if (spin.event_numbers) {
      sentence += "<span style='color:red'><b>[Ev " + i + "]</b></span> ";
    }
    if (typeof lastNarratedEvent !== "undefined" && event.start < lastNarratedEvent.start) {
      if (spin.time_markers) {
        sentence += choice(["Before that, ", "Previously, ", "Earlier, ", "Beforehand, "]);
        fix = false;
      } else {
        fix = true;
      }
      oldSpeaking = spin.speaking;
      oldReferring = spin.referring;
      if (spin.speaking === "after") {
        if (spin.referring === "posterior") { spin.referring = "simple"; }
        else if (spin.referring === "simple") { spin.referring = "anterior"; }
      }
      if (spin.speaking === "during") { spin.speaking = "after"; }
      if (spin.speaking === "before") { spin.speaking = "during"; }
      sentence += event.realize(spin, fix);
      if (!fix) { sentence += event.realize(spin).slice(-1); }
      spin.speaking = oldSpeaking;
      spin.referring = oldReferring;
    } else {
      sentence += event.realize(spin);
    }
    div.innerHTML = sentence;
    element.appendChild(div);
    lastNarratedEvent = event;
  }
  div = document.createElement("div");
  div.innerHTML = "The end.";
  element.appendChild(div);
}

// ### ARTICLES ###

const singularArticles = ["a", "an", "one"];
const pluralArticles = ["several", "many"];
// "the" or "some" can be used with singular or plural NPs


// ### PREPOSITIONS ###

var spatial = {
  in: "in",
  of: "possessed by", // Not exactly spatial, but it's here for now
  on: "on",
  partOf: "a part of",
  featureOf: "a feature of",
  far: "far from", // These would be used when the *only* relevant thing
  near: "near to"  // is mentioning an actor is far from/near to a place
};

var temporal = {
  at: "at",
  by: "by",
  down: "down",
  for: "for",
  from: "from",
  in: "in",
  into: "into",
  on: "on",
  outside: "outside",
  through: "through",
  to: "to",
  up: "up",
  upTo: "up to",
  using: "using",
  with: "with"
};

var number = { // Not currently used.
  singular: "one",
  plural: "several",
  unknown: "one or more"
};

var givens = new Set();

// ### UTILITY ###

function getParameters(actor) {
  var params = window.location.search, spin = {}, pair;
  if (params.substring(0, 1) === "?") {
    params = params.slice(1);
    params = params.split(",");
    for (var p of params) {
      pair = p.split("=");
      if (pair[0] === "narrator" || pair[0] === "narratee") { spin[pair[0]] = actor[pair[1]]; }
      else if (pair[0] === "time_markers") { spin.time_markers = true; }
      else if (pair[0] === "event_numbers") { spin.event_numbers = true; }
      else if (pair[0] === "expression_numbers") { spin.expression_numbers = true; }
      else { spin[pair[0]] = pair[1]; }
    }
  }
  if (!spin["speaking"]) { spin.speaking = "during"; }
  if (!spin["referring"]) { spin.referring = "simple"; }
  return spin;
}
