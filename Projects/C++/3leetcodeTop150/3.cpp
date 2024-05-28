#include <iostream>
#include <vector>

using namespace std;

class Solution {
public:
    int removeDuplicates(vector<int>& nums) {
        int j = 1;

        for (int i=1; i<nums.size(); i++) {
            if (nums[i] != nums[i-1]) {
                nums[j] = nums[i];
                j++;
            }
        }

        return j;
    }
};

int main() {
    Solution solution;
    
    vector<int> nums = {1,1,2};

    int output = solution.removeDuplicates(nums);

    for (int elem : nums) {
        cout << elem << " ";
    }
    cout << endl; // Print a newline at the end
    return 0;
}