interface Eq<T> {
  public Boolean eq (T t2);
}

public class MExpr<A extends Eq<A>> implements Eq<MExpr<A>>{
  @Override 
  public Boolean eq (MExpr<A> m) {
  	return true;
  }  

}
