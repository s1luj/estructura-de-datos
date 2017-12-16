import dataStructures.searchTree.*;
public class MainAugmentedBST {

	public static void main(String[] args) {
			AugmentedBST < Integer > bst = new AugmentedBST < Integer >() ;
			int xs [] = new int [] {20 ,10 ,30 ,5 ,15 ,25 ,35};
			for (int x : xs) {
				bst.insert(x);
			}
			System.out.println(bst);


	}

}
