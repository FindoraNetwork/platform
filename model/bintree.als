sig Value {}
abstract sig Color {}
sig Red extends Color {} { one Red }
sig Black extends Color {} { one Black }

sig BinNode {
  key: Int,
  val: Value,
  left:  lone BinNode,
  right: lone BinNode,
  color: Color
} {
  no ((left.*(@left+@right)) & (right.*(@left+@right)))
  this not in this.descendents
}

fun descendents[par: BinNode]: set BinNode {
  par.^(left + right)
}

fun treeFrom[par: BinNode]: set BinNode {
  par + par.^(left + right)
}

pred balanced[par: BinNode] {
  ((plus[1,plus[#(par.left.treeFrom),#(par.left.treeFrom)]]
      >= #(par.right.treeFrom)
    and #par.left.treeFrom < #par.right.treeFrom
   ) or
   (plus[1,plus[#(par.right.treeFrom),#(par.right.treeFrom)]]
      >= #(par.left.treeFrom)
    and (#(par.left.treeFrom) > #(par.right.treeFrom))
   ) or (#par.left.treeFrom = #par.right.treeFrom)
  )
}

sig BinTree {
  root: BinNode
}

pred inTree[node: BinNode, tree: BinTree] {
  node in tree.root.*(left+right)
}

pred isBSTNode[node: BinNode] {
  all n: (node.left.treeFrom) | n.key < node.key
  all n: (node.right.treeFrom) | n.key > node.key
}

pred isBST[t: BinTree] {
  all n: t.root.treeFrom | isBSTNode[n]
}

pred rbInvariant[n: BinNode] {
  some r: Red   | n.color = r => (all c: n.(left+right).color | c != Red)
  (some b: Black |
    (sum c: (n.left.treeFrom) |
      c.color = b => 1 else 0) =
    (sum c: (n.right.treeFrom) |
      c.color = b => 1 else 0)
  )
}

fact { one BinTree }
fact { all b: BinTree | isBST[b] }
fact { all n: BinNode | one t: BinTree | inTree[n,t] }
fact { all v: Value | some n: BinNode | v = n.val }

fact { all n: BinNode | rbInvariant[n] }

check { all b: BinNode | balanced[b] } for 10 but 6 Int

run { #BinNode > 7 } for 40 but 6 Int

