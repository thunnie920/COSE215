package kuplrg

trait Template {

  // Convert a PDA with final states to a PDA with empty stacks
  def pdafs2es(pda: PDA): PDA

  // Convert a PDA with empty stacks to a PDA with final states
  def pdaes2fs(pda: PDA): PDA

  // Convert a CFG to a PDA with empty stacks
  def cfg2pdaes(cfg: CFG): PDA
}
