musician(lennon, guitar).
musician(mccartney, guitar).
musician(harrison, guitar).
musician(starr, drums).
musician(lennon, voice).
musician(mccartney, voice).
musician(harrison, voice).
musician(starr, voice).

musician(page, guitar).
musician(plant, voice).
musician(jones, guitar).
musician(bonham, drums).

musician(gilmour, guitar).

%% X is a guitarist if they play guitar
guitarist(X) :- musician(X, guitar).
drummer(X) :- musician(X, drums).

band(beatles, lennon).
band(beatles, mccartney).
band(beatles, harrison).
band(beatles, starr).

band(zeppelin, page).
band(zeppelin, plant).
band(zeppelin, jones).
band(zeppelin, bonham).

%% X has a drummer if X is a band, and there is a member Y who is a drummer
has_drummer(X) :- band(X, Y), drummer(Y).
