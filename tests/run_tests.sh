#!/bin/sh
#======================================================================
# PACKTRACK - regresioni testovi
# Autor: Boris Janjetovic
#----------------------------------------------------------------------
# 1. Dnevna obrada od nule za 09.05.2020 -> izlazi se porede sa
#    "zlatnim" datotekama u tests/expected (bajt po bajt).
# 2. Ponovna obrada istih podataka nad postojećom matičnom ne smije
#    ništa promijeniti (nijedan događaj, nijedno obavještenje).
# 3. Datoteka kurira BRZ sa pogrešnim kontrolnim zbirom: serija se
#    odbacuje (RC=8), a ostali kuriri se normalno obrađuju.
#======================================================================
set -u
ROOT=$(cd "$(dirname "$0")/.." && pwd)
EXP=$ROOT/tests/expected
WORK=$ROOT/work
FAILS=0

pass() { echo "  OK     $*"; }
fail() { echo "  GRESKA $*"; FAILS=$((FAILS + 1)); }
run_job() {
    _log=$1; shift
    env PKT_RUN_DATE=20200509 "$@" "$ROOT/jcl/PKTDNEV.sh" > "$_log" 2>&1
}

mkdir -p "$WORK"

echo "TEST 1: dnevna obrada od nule, datum 2020-05-09"
run_job "$WORK/PKTDNEV.log" PKT_WORK="$WORK" PKT_FRESH=Y
RC=$?
# RC=4 je ocekivan: podaci namjerno sadrze neispravne redove
if [ "$RC" -eq 4 ]; then pass "MAXCC=4"; else fail "MAXCC=$RC, ocekivano 4"; fi
for f in "$EXP"/*; do
    n=$(basename "$f")
    if cmp -s "$f" "$WORK/$n"; then
        pass "$n"
    else
        fail "$n se razlikuje od ocekivanog:"
        diff "$f" "$WORK/$n" | head -20
    fi
done

echo "TEST 2: ponovna obrada istih podataka ne smije nista promijeniti"
cp "$WORK/EVTHIST.dat" "$WORK/EVTHIST.before"
run_job "$WORK/rerun.log" PKT_WORK="$WORK"
if grep -q "PRIMIJENJENO         :      0" "$WORK/rerun.log" \
   && [ ! -s "$WORK/NOTIFY.dat" ] \
   && cmp -s "$WORK/EVTHIST.before" "$WORK/EVTHIST.dat" \
   && grep -q "REGISTROVANO         :      0" "$WORK/rerun.log"
then
    pass "0 primijenjenih dogadjaja, 0 obavjestenja, istorija ista"
else
    fail "ponovna obrada je promijenila podatke"
fi

echo "TEST 3: pogresan kontrolni zbir u seriji kurira BRZ"
mkdir -p "$WORK/badtrailer"
run_job "$WORK/badtrailer.log" PKT_WORK="$WORK/badtrailer" PKT_FRESH=Y \
    PKT_BRZFILE="$ROOT/tests/data/badtrailer/brzaposta.dat"
RC=$?
if [ "$RC" -eq 8 ] \
   && grep -q "SERIJA ODBACENA" "$WORK/badtrailer.log" \
   && [ ! -s "$WORK/badtrailer/EVTBRZ.dat" ] \
   && [ -s "$WORK/badtrailer/EVTEUP.dat" ] \
   && grep -q "STEP040  PKTRPT  ZAVRSEN" "$WORK/badtrailer.log"
then
    pass "serija BRZ odbacena (RC=8), posao zavrsen do kraja"
else
    fail "ocekivan MAXCC=8 i nastavak obrade, dobijen RC=$RC"
fi

echo
if [ "$FAILS" -eq 0 ]; then
    echo "SVI TESTOVI PROSLI"
else
    echo "$FAILS TEST(OVA) PALO"
    exit 1
fi
