module QM = Requin_QM;
module Term = Requin_Term;

module Set = Relude.Set;

open Relude.Function.Infix;

let solve: list(int) => array(Term.t) = QM.solve >> Set.toArray;
