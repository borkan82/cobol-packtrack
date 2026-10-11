#!/bin/sh
#======================================================================
# PKTDNEV - DNEVNA OBRADA SISTEMA PACKTRACK (TAIPANSTUDIO)
# AUTOR   : BORIS JANJETOVIC
#----------------------------------------------------------------------
# Simulacija JCL posla. Svaki korak pokreće jedan COBOL program, a
# datoteke se dodjeljuju preko DD_* varijabli (kao DD kartice).
#
#   STEP010  PKTREG  - registracija novih pošiljaka iz manifesta
#   STEP020  PKTIMP  - uvoz događaja: BRZA POSTA   (fiksna dužina)
#   STEP021  PKTIMP  - uvoz događaja: EUROPAKET    (CSV)
#   STEP022  PKTIMP  - uvoz događaja: CITY KURIR   (polja sa "|")
#   STEP030  PKTUPD  - sort svih događaja, ažuriranje statusa,
#                      istorija i obavještenja za kupce
#   STEP040  PKTRPT  - SLA po kuriru i izuzeci
#
# Matična datoteka pošiljaka i istorija događaja OSTAJU između dva
# pokretanja (kao VSAM klaster na mainframe-u). Ponovno pokretanje
# sa istim podacima ništa ne mijenja: pošiljke su već registrovane,
# a događaji već primijenjeni.
#
# Varijable okruženja (opciono):
#   PKT_DATA      - ulazni podaci (default: ./data)
#   PKT_WORK      - izlaz, matična i istorija (default: ./work)
#   PKT_FRESH=Y   - obriši matičnu i istoriju (početak od nule)
#   PKT_BRZFILE   - alternativna datoteka kurira BRZ (za testove)
#   PKT_RUN_DATE  - datum obrade GGGGMMDD (default: današnji)
#======================================================================
set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
BIN=$ROOT/bin
DATA=${PKT_DATA:-$ROOT/data}
WORK=${PKT_WORK:-$ROOT/work}
MAXCC=0

mkdir -p "$WORK"

log() {
    echo "PKTDNEV $(date '+%H:%M:%S') $*"
}

# step <ime> <program> <max dozvoljeni RC>
step() {
    _name=$1; _prog=$2; _cond=$3
    log "$_name  EXEC PGM=$_prog"
    "$BIN/$_prog"
    _rc=$?
    log "$_name  $_prog  ZAVRSEN, RC=$_rc"
    [ "$_rc" -gt "$MAXCC" ] && MAXCC=$_rc
    if [ "$_rc" -gt "$_cond" ]; then
        log "*** POSAO PREKINUT U KORAKU $_name (RC=$_rc > $_cond) ***"
        log "PKTDNEV ZAVRSEN, MAXCC=$MAXCC"
        exit "$_rc"
    fi
}

log "===== START POSLA PKTDNEV ====="
log "PODACI: $DATA"
log "IZLAZ : $WORK"

#---- STEP000: brisanje dnevnih izlaza; matična samo uz PKT_FRESH=Y
log "STEP000  EXEC PGM=IEFBR14"
rm -f "$WORK"/EVT???.dat "$WORK"/EVTSRT.dat "$WORK"/NOTIFY.dat \
      "$WORK"/UPDRPT.txt "$WORK"/SLARPT.txt
if [ "${PKT_FRESH:-N}" = "Y" ]; then
    log "STEP000  PKT_FRESH=Y - BRISE SE MATICNA I ISTORIJA"
    rm -f "$WORK"/SHIPMAST.idx* "$WORK"/EVTHIST.dat
fi

#---- Zajedničke DD kartice
DD_SHIPMAST=$WORK/SHIPMAST.idx
export DD_SHIPMAST

#---- STEP010: registracija
DD_MANIN=$DATA/shipments.dat
export DD_MANIN
step STEP010 PKTREG 4

#---- STEP020..022: uvoz po kuriru. RC=8 (odbačena serija) je
#     upozorenje za cijeli posao, ostali kuriri se i dalje obrađuju.
export PKT_CARRIER DD_CARRIN DD_EVTOUT
PKT_CARRIER=BRZ
DD_CARRIN=${PKT_BRZFILE:-$DATA/brzaposta.dat}
DD_EVTOUT=$WORK/EVTBRZ.dat
step STEP020 PKTIMP 8
PKT_CARRIER=EUP
DD_CARRIN=$DATA/europaket.csv
DD_EVTOUT=$WORK/EVTEUP.dat
step STEP021 PKTIMP 8
PKT_CARRIER=CIT
DD_CARRIN=$DATA/citykurir.txt
DD_EVTOUT=$WORK/EVTCIT.dat
step STEP022 PKTIMP 8

#---- STEP030: ažuriranje statusa
DD_EVTBRZ=$WORK/EVTBRZ.dat
DD_EVTEUP=$WORK/EVTEUP.dat
DD_EVTCIT=$WORK/EVTCIT.dat
DD_EVTSRT=$WORK/EVTSRT.dat
DD_EVTHIST=$WORK/EVTHIST.dat
DD_NOTIFY=$WORK/NOTIFY.dat
DD_UPDRPT=$WORK/UPDRPT.txt
export DD_EVTBRZ DD_EVTEUP DD_EVTCIT DD_EVTSRT DD_EVTHIST \
       DD_NOTIFY DD_UPDRPT
step STEP030 PKTUPD 4

#---- STEP040: izvještaj
DD_SLARPT=$WORK/SLARPT.txt
export DD_SLARPT
step STEP040 PKTRPT 4

log "===== PKTDNEV ZAVRSEN, MAXCC=$MAXCC ====="
exit "$MAXCC"
