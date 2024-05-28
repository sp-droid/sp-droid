#include <iostream>
#include <vector>

using namespace std;

class Solution {
public:
    int removeDuplicates(vector<int>& nums) {
        int numberUniquesOrPairs = 1;
        int currentUniqueNumber = 1;
        
        for (int i=1; i<nums.size(); i++) {
            if (nums[i] != nums[i-1]) {
                currentUniqueNumber = 1;
                nums[numberUniquesOrPairs] = nums[i];
                numberUniquesOrPairs++;
            } else {
                currentUniqueNumber++;
                if (currentUniqueNumber <= 2) {
                    nums[numberUniquesOrPairs] = nums[i];
                    numberUniquesOrPairs++;
                }
            }
        }
        return numberUniquesOrPairs;
    }
};

int main() {
    Solution solution;
    
    vector<int> nums = {1,1,1,2,2,3};

    int output = solution.removeDuplicates(nums);

    for (int elem : nums) {
        cout << elem << " ";
    }
    cout << endl; // Print a newline at the end
    return 0;
}