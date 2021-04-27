// Trackatron
//
// A live coding language using the alphabet following the concept of a tracker.
//
// Every character is either an operator, or a value from 0 to ~maxsize.
// Any operator can be used to modulate any parameter of another operator.
// Values can either be fixed, patterns or modulated by other operators.
//
// Everything is an array where indices can be accessed with case
// UNsensitive charaters (with "[0-9a-zA-Z]".digit).

// op [[modulator] & value] ...
// ~trackatron.value("0 .f 1g");
// "0 .f 1g".tracatron;


// all characters and numbers (case UNsenstive)
~maxsize = ("z".digit + 10).sum;

//
~instruments = Array.newClear(~maxsize);
~instruments[0] = \acid;

// list of operators
~ops = Array.newClear(~maxsize);
~ops[0] = Pdef(\op0, Pbind(\instrument, \acid));

// list of patterns
~patterns = Array.newClear(~maxsize);

// list of parameter symbols
// each character is mapped to a parameter symbol
~params = Array.newClear(~maxsize);
~params["a".digit] = \amp;
~params["d".digit] = \dur;
~params["i".digit] = \instrument;
~params["f".digit] = \freq;
~params["g".digit] = \degree;

(
~trackatron = { |str|
	var params, op, instrument, freq, amp, dur, degree;
	// params = # op, freq, amp, dur, degree = str.split($ ).collect(_.digit);
	params = # op, instrument, freq, amp, dur, degree = str.split($ );
	str.debug("str");
	op.debug("op");
	// get pdef
	op = ~ops[op.digit.sum];

	params.do{|x,i|
		var all, param, mod, val;
		// x.debug(i);
		all = # param, mod, val = x.digit;
		// all.debug(i);
		param = ~params[param];
		// mod = mod.debug("mod"+i);
		// val = val.debug("val"+i);
		[param, mod, val].debug;
		op.set(param, val);
	};
	// Pdef((\op++op), Pbind(\instrument, \fmop)).play;
};

~str = "0 i.0 f.z d.1";
~trackatron.value(~str);
)

Loopier.boot;
Loopier.livecode;
s.plotTree;

"i.0".reshape(3,1)
~ops[1].class

~ops[0]
~ops[0].play
~ops[0].set(\amp, 0.2)
(dur: 1/2) <> ~ops[0]
~ops[0] <> (dur: 1/2)
Pdef(\ch,  nil , ~ops[0]).play;
s.freeAll

Pchain( ~ops[0], Pbind(\dur, 1/2));
Pchain( ~ops[0], Pbind(\dur, 1/2));
a = Array()
p = \freq
a = a++[p, 1]
a.pb.class
Pbind.fromArray
a = [\dur, 1/2]
("*"++a).cs
Pdef(\a, Pbind(\instrument, \acid)).play
Pdef(\a).set(\dur, 1/2)
~ops[0].set(\dur, 1/2)
1.