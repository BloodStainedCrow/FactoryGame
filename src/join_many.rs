// https://stackoverflow.com/a/67140319/15698509
macro_rules! join {
    // Entry point for `let <pat> = fork <closure>;` usage.
    ( $( let $lhs:pat = fork $rhs:expr ; )+ ) => {
        let join!( @left $( $lhs , )+ ) = join!( @right $( $rhs , )+ );
    };

    // Entry point for `<closure>,` usage.
    ( $x:expr $( , $xs:expr )* ) => {
        join! { @flat $x $( , $xs )* }
    };

    // Flattening tuples with temporary variables.
    ( @flat $( let $lhs:ident = $rhs:expr ; )+ ) => {
        {
            let join!( @left $( $lhs , )+ ) = join!( @right $( $rhs , )+ );
            ($( $lhs ),+)
        }
    };
    ( @flat $( let $lhs:ident = $rhs:expr ; )* $x:expr $( , $xs:expr )*) => {
        join! { @flat
            $( let $lhs = $rhs ; )*
            let lhs = $x;
            $($xs),*
        }
    };

    // Left hand side recursion to nest individual patterns into tuple patterns
    // like `(x, (y, (z, ...)))`.
    ( @left $x:pat , ) => {
        $x
    };
    ( @left $x:pat , $( $xs:pat , )+ ) => {
        ( $x , join!( @left $( $xs , )+ ) )
    };

    // Right hand side recursion to nest exprs into rayon fork-joins
    // like:
    //
    //     rayon::join(
    //         x,
    //         || rayon::join(
    //             y,
    //             || rayon::join(
    //                 z,
    //                 || ...)))
    ( @right $x:expr , ) => {
        ($x)()
    };
    ( @right $x:expr , $( $xs:expr , )+ ) => {
        ::rayon::join( $x , || join!( @right $( $xs , )+ ) )
    }
}
pub(crate) use join;
