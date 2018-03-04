def headOption: Option[A] = foldRight(None: Option[A])((h,_) => Some(h))

def headOption: Option[A] = foldRight(None[A])((a, _) => Some(a))
