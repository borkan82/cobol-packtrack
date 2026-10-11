# Packtrack — Universal Courier Package Tracking (COBOL)

**Owner:** TaipanStudio
**Author:** Boris Janjetovic

Packtrack gives web shops a single tracking view across courier companies. Each courier sends its tracking events in its own file format and with its own status codes. Packtrack imports them all, translates them into one set of universal statuses, keeps one master record per package, notifies recipients, and reports how each courier performs against its promised delivery time.

---

## Job flow — `PKTDNEV` (daily)

```
 shipments.dat ─► STEP010 PKTREG ──────────────────────────────┐
 (web shop manifests)  register, promised date (workdays)      │
                                                               ▼
 brzaposta.dat ─► STEP020 PKTIMP (BRZ) ─► EVTBRZ ─┐      ┌──────────────┐
  fixed width, H/D/T + control count              │      │  SHIPMAST    │
 europaket.csv ─► STEP021 PKTIMP (EUP) ─► EVTEUP ─┼─SORT─► indexed file │
  CSV ";", ISO dates                              │  ▲   │  key: track  │
 citykurir.txt ─► STEP022 PKTIMP (CIT) ─► EVTCIT ─┘  │   │  alt: carrier│
  "|" separated, DD.MM.YYYY                          │   └──────┬───────┘
                                           STEP030 PKTUPD ◄─────┤ READ/REWRITE
                                             │   │   │          │
                                 UPDRPT.txt ◄┘   │   └► NOTIFY.dat (e-mail queue)
                                                 └──► EVTHIST.dat (OPEN EXTEND)
                                                                │
                                           STEP040 PKTRPT ◄─────┘ START on alt key
                                                 │
                                                 └──► SLARPT.txt (SLA + exceptions)

   PKTDATE, PKTSTAT: subprograms called by all steps (CALL ... USING)
```

| Step | Program | What it does |
|------|---------|--------------|
| STEP010 | `PKTREG` | Registers new shipments from web shop manifests into the indexed master: validates the courier (`SEARCH ALL`), computes the promised delivery date as ship date + N **working days** per courier, rejects duplicates via `INVALID KEY`. Creates the master on the first run (file status 35). |
| STEP020–022 | `PKTIMP` | Runs once per courier. Parses three different formats: fixed-width with header/detail/trailer and **batch control count** (`REDEFINES` per record type), semicolon CSV and pipe-separated (`UNSTRING ... TALLYING`). Converts dates through `PKTDATE` and status codes through `PKTSTAT`, and writes one normalized event format. A batch whose trailer count does not match is rejected as a whole (RC=8), and the job continues with the other couriers. |
| STEP030 | `PKTUPD` | Sorts the events of all couriers (`SORT ... USING` three files) into one timeline. For each event it reads the package by key and updates it **in place** (`REWRITE`, VSAM-style). Ignores late scans after delivery, duplicates and events from the wrong courier. Counts failed delivery attempts, appends applied events to the history (`OPEN EXTEND` on an `OPTIONAL` file), and queues recipient e-mails. |
| STEP040 | `PKTRPT` | Reads the master **by alternate key** (`START` on courier), so the per-courier control break needs no sort. Produces the SLA table (delivered on time / late, returned, in progress, on-time %, average days), a status summary, and an exception list: damaged, 3 failed attempts, no scan for N days, promised date missed. |

### Subprograms

| Module | Called as | Purpose |
|--------|-----------|---------|
| `PKTDATE` | `CALL 'PKTDATE' USING DATE-PARM` | `CNV`: any courier date/time format → validated `YYYYMMDDHHMM`. `ADD`: add N working days (skips weekends). |
| `PKTSTAT` | `CALL 'PKTSTAT' USING STAT-PARM` | `M`: courier + courier code → universal status via a sorted table and **binary search** (`SEARCH ALL`). `T`: status text and whether it is final. |

The parameter areas are copybooks (`DATEPARM`, `STATPARM`) shared by the caller's `WORKING-STORAGE` and the subprogram's `LINKAGE SECTION`. The Makefile links them statically. Dynamic `CALL` works unchanged with `cobc -m` and `COB_LIBRARY_PATH`.

## Universal statuses

| Code | Meaning | BRZA POSTA | EUROPAKET | CITY KURIR |
|------|---------|-----------:|----------:|-----------:|
| RG | registered | — | — | — |
| PU | picked up | 01 | PICKUP | P |
| TR | in transit | 10 | TRANSIT | T |
| HB | at sorting hub | 20 | HUB_IN | S |
| OD | out for delivery | 30 | OUT_DEL | O |
| DL | **delivered** (final) | 40 | DELIV | D |
| FA | failed attempt | 50 | NO_ONE | F |
| RT | **returned** (final) | 60 | RETURN | R |
| EX | damaged / exception | 90 | DAMAGE | X |

Adding a courier means one row in the courier table (`PKTCONST`), its codes in `PKTSTAT`, and a parsing paragraph in `PKTIMP`.

## Running it

```sh
make            # compile (subprograms linked into each program)
make run        # daily job from scratch, output in work/
make test       # golden files + rerun safety + rejected batch
```

```sh
docker build -t packtrack .
docker run --rm -v "$PWD/work:/app/work" packtrack
docker run --rm packtrack make test
```

Set `PKT_RUN_DATE=YYYYMMDD` to run "as of" a given day; the sample data is meant for **2020-05-09**. The master file and event history persist between runs, like a VSAM cluster. `PKT_FRESH=Y` starts from an empty system.

## Sample output

SLA by courier (`SLARPT.txt`):

```
 KURIRSKA SLUZBA         UKUPNO ISPORUCENO NA VRIJEME      KASNO    VRACENO     U TOKU % NA VRIJ. PROSJ.DANA
 BRZ BRZA POSTA               5          2          1          1          0          3      50.0%        3.5
 CIT CITY KURIR               5          3          2          1          0          2      66.7%        2.0
 EUP EUROPAKET                5          2          2          0          1          2     100.0%        4.0
```

Exceptions (same report, page 2):

```
 POSILJKA       KUR NARUDZBA   PRIMALAC               ST ZADNJI DOGADJAJ  ROK        RAZLOG
 BP260000000103 BRZ WEB-100260 DRAGAN SIMIC           TR 05.05.2020 22:00 07.10.2020 BEZ SKENIRANJA 4 D; KASNI 2 D;
 BP260000000104 BRZ WEB-100271 ANA MARKOVIC           FA 09.05.2020 12:00 08.10.2020 3 POKUSAJA - VRATITI; KASNI 1 D;
 EP260000000204 EUP WEB-200041 PETAR JOVIC            EX 08.05.2020 09:15 12.10.2020 OSTECENA/IZUZETAK;
```

All outputs of the sample run are in `tests/expected/`. The reports are print files, so pages are separated by form-feed characters.

## Tests

`make test` runs three scenarios:

1. **Daily run from scratch.** Every output (normalized events, history, notifications, both reports) is compared byte by byte with the golden files.
2. **Rerun with the same input.** No new registrations, no applied events, no notifications, and an unchanged history. The batch is safe to restart.
3. **Corrupted courier batch.** The BRZ trailer count does not match, so that batch is rejected (MAXCC=8). The other couriers are still processed and the job finishes.

The sample data deliberately contains a duplicate shipment, an unknown courier, an invalid date, an unknown status code, a truncated line, an event for an unknown package, a duplicate event, a late scan after delivery, and an event reported by the wrong courier.

## Record layouts (short)

| Record | Length | Contents |
|--------|-------:|----------|
| `SHPREC` | 200 | tracking no. (key), courier (alt key, duplicates), order, client, recipient, e-mail, ship/promised date, status + timestamp, location, failed attempts, delivered timestamp, event count |
| `MANREC` | 140 | manifest line from a web shop |
| `BRZREC` | 60 | courier BRZ: H/D/T records `REDEFINES`d over one data area |
| `EVTREC` | 100 | normalized event: tracking no., timestamp, courier, original code, universal status, location, note |
| `NTFREC` | 200 | queued recipient notification (channel, contact, message) |
