foreach section a {
  foreach relocation entry r {
    refptr = s + r.offset;	/* ptr to reference to be relocated */

    /* relocate a pc-relative reference */
    if (r.type == R_X86_64_PC32) {
      refaddr = ADDR(s) + r.offset; /* ref's run-time address */
      *refptr = (unsigned) (ADDR(s.symbol) + r.addend - refaddr);
    }

    /* relocate an absolute reference */
    if (r.type == R_X86_64_32)
      *refptr = (unsigned) (ADDR(r.symbol) + r.addend);
  }
}
