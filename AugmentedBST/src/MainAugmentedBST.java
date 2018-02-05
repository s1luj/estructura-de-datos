import dataStructures.searchTree.*;
public class MainAugmentedBST {

	public static void main(String[] args) {
			AugmentedBST < Integer > bst = new AugmentedBST < Integer >() ;
			int xs [] = new int [] {20 ,10 ,30 ,5 ,15 ,25 ,35};
			for (int x : xs) {
				bst.insert(x);
			}
			System.out.println(bst);
			
//prueba el select			resultados = 5 10 25 35 null
			System.out.println(bst.select(0));
			System.out.println(bst.select(1));
			System.out.println(bst.select(4));
			System.out.println(bst.select(6));
			System.out.println(bst.select(10));
			
			
// prueba el floor			resultados = null 5 5 5 10 25 35
/*			System.out.println(bst.floor(4));
			System.out.println(bst.floor(5));
			System.out.println(bst.floor(6));
			System.out.println(bst.floor(9));
			System.out.println(bst.floor(10));
			System.out.println(bst.floor(27));
			System.out.println(bst.floor(50));
*/
			
// prueba el ceiling 		resultados = 5 5 10 10 10 30 null
/*			
			System.out.println(bst.ceiling(4));
			System.out.println(bst.ceiling(5));
			System.out.println(bst.ceiling(6));
			System.out.println(bst.ceiling(9));
			System.out.println(bst.ceiling(10));
			System.out.println(bst.ceiling(27));
			System.out.println(bst.ceiling(50));
*/

// prueba el rank 		Resultados: 0 0 1 1 1 5 7
/*			System.out.println(bst.rank(4));
			System.out.println(bst.rank(5));
			System.out.println(bst.rank(6));
			System.out.println(bst.rank(9));
			System.out.println(bst.rank(10));
			System.out.println(bst.rank(27));
			System.out.println(bst.rank(50));
*/
			
// prueba el size 		Resultados: 0 1 1 1 1 2 5 5 0 7 0
/*			System.out.println(bst.size(1,4));
			System.out.println(bst.size(1,5));
			System.out.println(bst.size(5,5));
			System.out.println(bst.size(5,6));
			System.out.println(bst.size(1,9));
			System.out.println(bst.size(1,10));
			System.out.println(bst.size(1,27));
			System.out.println(bst.size(10,30));
			System.out.println(bst.size(40,50));
			System.out.println(bst.size(5,35));
			System.out.println(bst.size(35,5));
*/			
			
	}

}
