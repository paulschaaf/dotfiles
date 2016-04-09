; ----------------------------------------------------------------------
; -------------------- Rooms

(make-room "West of House"
"You are standing in an open field west of a white house, with a boarded front
door.")

(setf (entrance *dungeon*) '|West of House|)

(make-room "North of House"
"You are facing the north side of a white house. There is no door here, and all the windows are boarded up. To the north a narrow path winds through the trees.")

(make-room "Behind House"
"You are behind the white house. A path leads into the forest to the east.")

(make-room "Kitchen"
"You are in the kitchen of the white house. A table seems to have been used recently for the preparation of food. A dark chimney leads down.")

(make-room "Living Room"
"You are in the living room. There is a wooden door with strange gothic lettering to the west, which appears to be nailed shut, a trophy case, and a large oriental rug in the center of the room.")

(setf *attic* (make-room "Attic"
"This is the attic."))

; ----------------------------------------------------------------------
; -------------------- Paths

(make-path :from '|West of House|
  :type   'path
  :going  '(north)
  :to     '|North of House|
  :desc   "")

(make-path :from '|North of House|
  :type   'path
  :going  '(west)
  :to     '|West of House|
  :desc   "")

(make-path :from '|North of House|
  :type   'path
  :going  '(east)
  :to     '|Behind House|
  :desc   "")

(make-path :from '|Behind House|
  :type   'path
  :going  '(north)
  :to     '|North of House|
  :desc  "")

(make-path :from '|Behind House|
  :type   '|In one corner of the house there is a small window.|
  :going  '(in west)
  :to     '|Kitchen|
  :desc   "The window is slightly ajar")

(make-path :from '|Kitchen|
  :type   'window
  :going  '(out east)
  :to     '|Behind House|
  :desc   "To the east is a small window which is open.")

(make-path :from '|Kitchen|
  :type   'doorway
  :going  '(west)
  :to     '|Living Room|
  :desc   "A doorway leads to the west.")

(make-path :from '|Kitchen|
  :type   'staircase
  :going  '(up)
  :to     '|Attic|
  :desc   "A dark staircase can be seen leading upward.")

(make-path :from #[Living Room]
  :type   'doorway
  :going  '(east)
  :to     '|Kitchen|
  :desc   "There is a doorway to the east.")

(make-path :from '|Attic|
  :type   'stairway
  :going  '(down)
  :to     '|Kitchen|
  :desc   "The only exit is a stairway leading down.")

(make-container "mailbox"
		"It is rusty, and leans slightly to the left.")
(put-in '|West of House| '|mailbox|)

'(setf (immobile-because '|mailbox|) "It is securely anchored.")

(make-object "leaflet"
	     "~%\"WELCOME TO ZORKSim!~%ZORKSim is a game of adventure, danger, and low cunning. In it you will explore some of the most amazing territory ever seen by mortals. No computer should be without one!\"")

(put-in '|mailbox| '|leaflet|)

(make-object "broom" "It is a roughly-cut shaft of unpolished wood, with a bundle of twigs loosely tied to one end.")

(put-in *avatar* '|broom|) 

(make-object-in (location '|mailbox|)
	     "apple"
	     "It is firm and dark red, with a brown stem with a small green leaf coming out of the top.")

(setf (is-open (find-obj '|mailbox|)) nil)

(setf *burlap-sack* (make-bag-in *avatar*
	  "sack"
	  "It is made of worn brown burlap and smells slightly of garlic."))

;(setf (immobile-because *burlap-sack*) "As you are about to set it down, you decide that you'd better hold on to it instead.")

(make-object-in *burlap-sack*
	     "sword"
	     "It is made of highly-polished steel, with an ornate wooden handle.")

(make-object-in *burlap-sack*
	     "clove of garlic"
	     "You see nothing special about the garlic.")

(setf *purse* (make-bag-in *burlap-sack*
	  "purse"
	  "It is small and dirty, with a drawstring around the top."))

(make-object-in *purse*
	     "Euro"
	     "The coin is made of tarnished gold and has milled edges.")
