sig Value {}

fun max[a: Int, b: Int]: Int { a > b => a else b }
fun min[a: Int, b: Int]: Int { a < b => a else b }

fun defHeight[x: lone BinNode]: Int { some x => x.data.height else 0 }
fun defShortest[x: lone BinNode]: Int { some x => x.data.shortest else 0 }
fun defBlackHeight[x: lone BinNode]: Int { some x => x.data.blackHeight else 0 }

sig NodeData {
  height: Int,
  shortest: Int,
  blackHeight: Int
}

abstract sig BinNode {
  key: Int,
  val: Value,
  left:  lone BinNode,
  right: lone BinNode,
  data: NodeData
} {
  no ((left.*(@left+@right)) & (right.*(@left+@right)))
  this not in this.descendents
  data.height = plus[1,max[left.defHeight,right.defHeight]]
  data.shortest = plus[1,min[left.defShortest,right.defShortest]]
}

sig RedNode extends BinNode {}
{ data.blackHeight = max[left.defBlackHeight,right.defBlackHeight] }
sig BlackNode extends BinNode {}
{ data.blackHeight = plus[1,max[left.defBlackHeight,right.defBlackHeight]] }

fun descendents[par: BinNode]: set BinNode {
  par.^(left + right)
}

fun treeFrom[par: BinNode]: set BinNode {
  par + par.^(left + right)
}

pred balanced[par: BinNode] {
  (let smaller = par.defShortest,
       larger  = par.defHeight |

       plus[plus[smaller,smaller],1] >= larger
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
  all r: RedNode | no (r.(left+right) & RedNode)
  n.left.defBlackHeight = n.right.defBlackHeight
}

pred rbInvariant2[n: BinNode] {
  all r: RedNode | no (r.(left+right) & RedNode)
  #((n.left.treeFrom)&BlackNode) = #((n.right.treeFrom)&BlackNode)
}

fact { one BinTree }
fact { all b: BinTree | isBST[b] }
fact { all n: BinNode | one t: BinTree | inTree[n,t] }
fact { all v: Value | some n: BinNode | v = n.val }

// fact { all n: BinNode | rbInvariant[n] }
pred isRbTree[] { all n: BinNode | rbInvariant[n] }
fact { all n: BinTree | n.root in BlackNode }

check InvsEquiv {
  (all n: BinNode | rbInvariant[n]) <=> (all n: BinNode | rbInvariant2[n])
} for 7 but 6 int expect 1

run { isRbTree[] and #BinNode > 5 } for 7 but 6 Int
run { isRbTree[] and #BinNode > 5 and (some b: BinTree | some
(b.root.(left+right)&RedNode)) } for 7 but 6 Int

check RbHeightBound { (all b: BinNode | rbInvariant[b]) => (all b: BinNode | balanced[b]) } for 8 but 6 Int

run { isRbTree[] and #BinNode > 6 } for 8 but 6 Int

