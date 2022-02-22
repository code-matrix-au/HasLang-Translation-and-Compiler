/**
 * HasLang to SEC translator.
 *
 * Copyright 2021, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package haslang

/**
 * Translator from HasLang source programs to SEC target programs.
 */
object Translator {

    import SECTree._
    import HasLangTree._
    import scala.collection.mutable.ListBuffer

    /**
     * Return a frame that represents the SEC instructions for a FunLang program.
     */
    def translate (program : Program) : Frame = {

        // An instruction buffer for accumulating the program instructions
        val programInstrBuffer = new ListBuffer[Instr] ()

        /**
         * Translate the program by translating its expression.
         */
        val expInstrs = translateExpression (program.exp)
        programInstrBuffer.appendAll (expInstrs)
        programInstrBuffer.append (IPrint ())

        // Gather the program's instructions and return them
        programInstrBuffer.result ()

    }

    /**
     * Translate an expression and return a list of the instructions that
     * form the translation.
     */
    def translateExpression (exp : Exp) : Frame = {

        // An instruction buffer for accumulating the expression instructions
        val expInstrBuffer = new ListBuffer[Instr] ()

        /**
         * Generate an instruction by appending it to the instruction buffer.
         */
        def gen (instr : Instr) {
            expInstrBuffer.append (instr)
        }

        /**
         * Generate a sequence of instructions by appending them all to the
         * instruction buffer.
         */
        def genall (frame : Frame) {
            expInstrBuffer.appendAll (frame)
        }

        /**
         * Generate code to make a closure (argName => body).
         */
        def genMkClosure (argName : String, body : Exp) {
            val bodyInstrs = translateExpression (body)
            gen (IClosure (argName, bodyInstrs :+ IPopEnv ()))
        }

        exp match {

        case IdnUse (value) =>
            gen (IVar (value))

        case IntExp (value) =>
            gen (IInt (value))

        case BoolExp (value) =>
            gen (IBool (value))

        case PlusExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (IAdd ())

        case MinusExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (ISub ())

        case SlashExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (IDiv ())

        case StarExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (IMul ())

        case LessExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (ILess ())

        case EqualExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (IEqual ())

        case LamExp (IdnDef (name, _), body) =>
            genMkClosure(name, body)

        case ConsExp (l, r) =>
            genall (translateExpression (l))
            genall (translateExpression (r))
            gen (ICons ())

        case IfExp(cond, thenExp, elseExp) => 
                genall(translateExpression(cond))
                gen(IBranch (translateExpression(thenExp), translateExpression(elseExp)))
     
        case AppExp(fun, arg) =>
        if (fun == IdnUse("head")) {
          genall(translateExpression(arg))
          gen(IHead())
        } else if (fun == IdnUse("tail")) {
          genall(translateExpression(arg))
          gen(ITail())
        } else if (fun == IdnUse("length")) {
          genall(translateExpression(arg))
          gen(ILength())
        } else {
          genall(translateExpression(fun))
          genall(translateExpression(arg))
          gen(ICall())
        }
           
        case ListExp(exp) =>
            exp match {
                case Vector() => gen(INil())
                case h +: t =>
                    genall(translateExpression(h))
                    genall(translateExpression(ListExp(t)))
                    gen(ICons())
            }
            case LetExp(defn, exp) =>
        if (defn.length <= 1) {
          genall(
            translateExpression(
              AppExp(LamExp(defn.head.idndef, exp), defn.head.lines.head.exp)
            )
          )
        } else {
          genall(
            translateExpression(
              LetExp(Vector(defn.head), LetExp(defn.tail, exp))
            )
          )
        }
  
            
        // FIXME
        // handle:
        //    IfExp
        //    AppExp - "head" exp
        //           - "tail" exp
        //           - "length" exp
        //           - all other: exp exp
        //    ListExp
        //    LetExp

        case _ =>
            gen (IPrint ())
        }

        // Gather the expression's instructions and return them
        expInstrBuffer.result ()

    }

}
