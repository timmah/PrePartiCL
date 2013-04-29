PrePartiCL
==========
This repository holds code fragments that demonstrate a few of the techniques we plan to use with PartiCL.
The first demonstrates the use of Template Haskell to vectorize a Haskell record type.
That is, it takes a record type with scalar fields (instances of Data.Vector.Unbox) and returns a record type with vector fields.

## Template Haskell example of vectorization
The directory TH contains a simple example of vectorization by means of template Haskell.
The file VecXForm.hs contains the template Haskell code that takes a record type with scalar fields and generates a corresponding record type with vector fields.
The file Demo.hs shows a simple application of this.
At line 15, Demo defines a record type **MyT** with two fields.
The first field is named **gd**; it has type Double.
The second field is named **gi**; it has type Int.

Line 19 defines an instance of type **MyT**.

Line 24 invokes our Template Haskell routine to create a second type, MyTVectorized. It creates a type with two fields.
The first field is named **gdV**; it has type Vector Double.
The second field is named **giV**; it has type Vector Int.

Line 28 invokes a second bit of Template Haskell: this generates an instance of the Show class for the new type **MyTVectorized**. (The Show class basically permits instances to be converted to a String).

Line 30 uses the new type to create a vectorized instance.

One might want to experiment with this a bit in the Haskell interpreter.
Start the interpreter, GHCi, with Template Haskell enabled:

    $ ghci -XTemplateHaskell

Load the Demo module:

    Prelude> :l Demo.hs
    [1 of 2] Compiling VecXForm         ( VecXForm.hs, interpreted )
    [2 of 2] Compiling Demo             ( Demo.hs, interpreted )
    Loading package pretty-1.1.1.0 ... linking ... done.
    Loading package array-0.4.0.1 ... linking ... done.
    Loading package deepseq-1.3.0.1 ... linking ... done.
    Loading package containers-0.5.0.0 ... linking ... done.
    Loading package primitive-0.5.0.1 ... linking ... done.
    Loading package vector-0.10.0.1 ... linking ... done.
    Loading package template-haskell ... linking ... done.
    Ok, modules loaded: Demo, VecXForm.

Now print out **mytv**:

    Demo> mytv
    gdV = fromList [3.14159,6.28318] :: AppT (ConT Data.Vector.Unboxed.Base.Vector) (ConT GHC.Types.Double),
    giV = fromList [42,43,44,45,46,47,48,49,50,51,52,53] :: AppT (ConT Data.Vector.Unboxed.Base.Vector) (ConT GHC.Types.Int)

The type of **mytv** is **MyTVectorized**:

    Demo> :type mytv
    mytv :: MyTVectorized

