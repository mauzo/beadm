\ vi: set syn=forth :
vocabulary zfs-be
only forth also support-functions also zfs-be definitions

: 2roll ( x x' y y' ... z z' n -- y y' ... z z' x x' )
    ?dup 0= if exit then
    2* 1+ dup >R    
    roll R> roll       
;

: /string ( ca u n -- ca+n u-n )
    dup >R - swap   ( ca u n -- u-n ca )    ( R: -- n )
    R> + swap       ( u-n ca -- ca+n u-n )  ( R: n -- )
;

: zfspfx    s" zfs:" ;
: currdev   s" currdev" ;
: vfsroot   s" vfs.root.mountfrom" ;

\ Creates a newly-allocated string with the contents of the n given
\ strings, in stack order (so, 's" one" s" two" 2 join' gives "onetwo").
: join ( ca u ... n -- ca' u' )
    2* dup >R           ( n -- 2n ) ( R: -- 2n )
    0 swap 1 do         ( 2n -- 0 2n 1 -- 0 )
        i pick +        ( ca u ... total -- ca u ... total+u )
    2 +loop

    R> swap dup >R      ( u' -- 2n u' ) ( R: 2n -- u' )
    chars allocate if ENOMEM throw then   ( 2n u' -- 2n ca' )
    dup >R              ( R: u' -- u' ca' )

    2 rot do            ( 2n ca' -- ca' 2 2n -- ca' )
        i roll swap     ( ca u ... ca' -- u ... ca ca' )
        i roll          ( u ... ca ca' -- ... ca ca' u )
        2dup 2>R        ( ca ca' u ) ( R: * -- * ca' u )
        \ this ought to be cmove, but ficl doesn't have that
        move 2R> +      ( ca ca' u -- ca'' ) ( R: * ca' u -- * )
    -2 +loop
    drop R> R>          ( ca'' -- ca' u' ) ( R: u' ca' -- )
;

\ These mostly throw -1 on failure.
\ There doesn't appear to be any way to do a block with early exit
\ except by making a new word. FICL doesn't support the ANS extensions
\ for building control-flow macros (cs-pick, cs-roll) and gets quite
\ upset if I try to emulate them.

: match-prefix ( ca1 u1 ca2 u2 -- ca3 u3 )
    begin
        dup 0>
    while
        2 pick 0= throw
        1 pick c@ 4 pick c@ <> throw
        1 /string 2swap 1 /string 2swap
    repeat
    2drop
;

: find-slash ( ca u -- ca' u' )
    begin
        dup 0= throw
        1 pick c@ >R
        1 /string R>
        dup [char] : = throw 
        [char] / <> 
    while repeat
;

\ The returned string is read-only and should not be freed.
: be-root ( -- ca u )
    currdev getenv                      ( ca u )
    dup -1 = throw
    zfspfx match-prefix                 ( ca u -- ca' u' )
    2dup find-slash find-slash          ( ca u -- ca u ca' u' )
    drop 2 pick                         ( ca u ca' ca )
    - ( ca u u' ) swap drop             ( ca u' )
;

: disable-all-modules ( -- )
    module_options @
    begin
        ?dup
    while
        dup module.flag @ if
            false over module.flag !
        then
        module.next @
    repeat
;

\ The loader command 'set' is not directly callable from compiled code,
\ because it's a builtin and reads its arguments from TIB. The Forth
\ setenv is not the same, and doesn't give error messages when setting
\ invalid values (e.g. a currdev which doesn't exist).
: xset ( val u var u -- )
    s" set " 2swap          ( val set var )
    s' ="' 3 2roll s' "'    ( set var =" val " )
    5 join over >R          ( cmd u ) ( R: -- cmd )
    evaluate                ( cmd u -- )
    R> free throw           ( R: cmd -- )
;

\ We need to call the interactive initialize, which lives in
\ forth-wordlist. There is another initialize in support-functions, but
\ that does something different. Also, initialize seems to leave random
\ junk on the stack. (Oh for cleartomark...)
: call-initialize ( -- )
    depth >R
    [ also forth ] initialize [ previous ]
    depth R> do drop loop
;

\ Takes a BE name. Returns the full path to that BE, with zfs: prefix.
\ The returned string is newly allocated.
: be-resolve ( ca u -- ca' u' )
    be-root                 ( be -- be root )
    zfspfx 2swap 2 2roll    ( be root -- zfs root be )
    3 join
;

\ Takes the full path to a BE and activates it. Setting currdev will
\ throw if the BE does not exist.
: (be-activate) ( ca u -- )
    2dup s" :" 2 join over >R
    currdev xset
    R> free throw
    disable-all-modules
    call-initialize
    vfsroot xset
;

: be-activate ( argv... argc true | false "be" -- f )
    0= if ( interpreted ) get_arguments then
    1 <> if abort" Usage: be-activate BE" then

    ['] be-resolve catch if
        abort" You do not appear to be booting from a BE."
    then

    2dup ['] (be-activate) catch if
        abort" Activating BE failed!"
    then

    over >R
    ." Activated BE " type ." ." cr
    R> free     \ builtin: will check this
;

also forth definitions also builtins

builtin: be-activate

only forth

