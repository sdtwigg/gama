package gama
package internal
package frontend

class TyperWidthInferer(target: EnclosingModule) {
  def infer: EnclosingModule = {
    // 1. Build up table of uninferred widths and associated constraints
    //   Store as: Map[ExprHW -> Seq[TypeTrace]], Map[TypeTrace -> Seq[Constraints]]
    //   Constraints: >=Int, >=TypeTrace, >=(Op,TypeTrace,TypeTrace)
    //  -> Will have to walk the entire tree, particularly expressions and connections
    //  -> Maybe reasonanble to do a quick walk to populate first map so can skip detailed walks
    //       e.g.
    //  -> Also, will want to ensure AliasDecl is carefully handled...
    //     a. AliasDecl can act like ConstDecl where symbol constrained by ref
    //     b. HOWEVER: for ConnectStmt.sink, BiConnectStmt.left,right, constraints must be built off de-aliased
    //        Thus, will need to build de-aliasing table: Map[RefSymbol -> RefHW]
    // 2. Resolve the constraints in the second map to make result Map[TypeTrace -> Int] (or ->Option[Int]?)
    // 3. Build replacement Map[ExprHW -> ExprHW] by using first map and result map
    //  -> Can be clever and decompose the TypeTrace, group up constituent parts, and then do cloning

    // Note: A traversal function:
    //     constrainFrom(targetT: TypeHW, targetP: TypeTrace, modelT: TypeHW, modelP: TypeTrace)
    //   that just ensures the targetT is constrained to be >= the modelT will likely do most of the work
    // Also, the Connect and BiConnect need their own special walk that uses details
    // Multiplication will be annoying and require the arithmetic between possibly 2 TypeTraces
    ???
  }
}
