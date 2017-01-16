;
; Top assembly file for SJASM compiler
; Patched version of CP/M
;

    module CPM
    code @ $e000..$f5fff
    include "cp-m/fdd3000_cpm_patched.asm"


    module BIOS
    code ! $f600
    include "bios/fdd3000_cpm_bios.asm"

    end
