package com.wolfskeep

class SelfModifyingCodeException(val finger: Int) extends Exception(s"Self-modifying code detected at finger $finger")