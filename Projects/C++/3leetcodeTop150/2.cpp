#include <iostream>
#include <vector>

using namespace std;

class Solution {
public:
    int removeElement(vector<int>& nums, int val) {
        int unequalElements = 0;
        const int size = nums.size();
        
        for (int i=0; i<size; i++) {
            const int value = nums[i];
            if (value != val) {
                nums[unequalElements] = value;
                unequalElements++;
            }
        }
        
        return unequalElements;
    }
};

int main() {
    Solution solution;
    
    vector<int> nums = {3,2,2,3};
    int val = 3;

    int output = solution.removeElement(nums, val);

    for (int elem : nums) {
        cout << elem << " ";
    }
    cout << endl; // Print a newline at the end
    return 0;
}