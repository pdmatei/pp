interface Tree<T> {
	public Integer size();
	public Tree<T> mirror();
	public <Tp> Tree<Tp> map(Func<T,Tp> f);
}
interface Func<A,B>{
	public B call (A a);
}


class Void<T> implements Tree<T> {
	
	public Void () {}
	@Override 
	public Integer size () {return 0;}
	@Override 
	public Tree<T> mirror () {return this;}
	@Override
	public<Tp> Tree<Tp> map (Func<T,Tp> f) {return new Void<Tp>();}
}

class Node<T> implements Tree<T> {
	private Tree l,r;
	private T i;
	public Node (Tree l, T i, Tree r){
		this.l = l; this.i = i; this.r = r;
	}	

	@Override
	public Integer size() {return 1 + l.size() + r.size();}

	@Override
	public Tree<T> mirror () {return new Node<T>(r.mirror(),i,l.mirror());}

	@Override 
	public <Tp> Tree<Tp> map (Func<T,Tp> f){
		return new Node<Tp>(l.map(f),f.call(i),r.map(f));
	}
}


public class Trees {}