       01  REXTORE.
           05  COLLECTIE-ID PIC 999.
           05  COLLECTIE-NAME PIC X(64) VALUE 'PENDING'.
           05  SPECIES PIC X(64) VALUE 'Rextore'.
           05  COLLECTIE-TYPE PIC X(64) VALUE 'DINOSAUR'.
           05  SOUND PIC X(64) VAlUE 'rarwar'.
           05  PREFERRED-BIOME PIC X(64) VALUE 'VOLCANIC'.
           05  ATTACK-POWER PIC 999 VALUE 10.
           05  DEFEND-FLOOR PIC 999 VALUE 50.
           05  GRAPHIC PIC X(1024) VALUE 
       "        ," & X'0A' & 
       "       /|"  & X'0A' & 
       "      / |"  & X'0A' & 
       "     /  /"  & X'0A' & 
       "    |   |"  & X'0A' & 
       "   /    |"  & X'0A' & 
       "   |    \\_"  & X'0A' & 
       "   |      \\__"  & X'0A' & 
       "   \\       __\\_______"  & X'0A' & 
       "    \\                 \\_"  & X'0A' & 
       "    | /                 \\"  & X'0A' & 
       "    \\/                   \\"  & X'0A' & 
       "     |                    |"  & X'0A' & 
       "     \\                   \\|"  & X'0A' & 
       "     |                    \\"  & X'0A' & 
       "     \\                     |"  & X'0A' & 
       "     /\\    \\_               \\"  & X'0A' & 
       "    / |      \\__ (   )       \\"  & X'0A' & 
       "   /  \\      / |\\\\  /       __\\____"  & X'0A' & 
       "snd|  ,     |  /\\ \\ \\__    |       \\_"  & X'0A' & 
       "   \\_/|\\___/   \\   \\}}}\\__|  (@)     )"  & X'0A' & 
       "    \\)\\)\\)      \\_\\---\\   \\|       \\ \\"  & X'0A' & 
       "                  \\>\\>\\>   \\   /\\__o_o)"  & X'0A' & 
       "                            | /  VVVVV"  & X'0A' & 
       "                            \\ \\    \\"  & X'0A' & 
       "                             \\ \\MMMMM  "  & X'0A' & 
       "                              \\______/".


