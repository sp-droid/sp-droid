#include <iostream>
#include <unordered_map>
#include <vector>

using namespace std;

class Solution {
public:
    bool canJump(vector<int>& nums) {
        bool result = true;
        int jump = 0;
        const int n = nums.size();
        if (n == 1) { return true; }

        if (nums[n-1] == 0) {
            result = false;
            jump = 0;
        }
        for (int i=n-2; i>=0; i--) {
            const int value = nums[i];
            if (result == true) {
                if (value == 0) {
                    result = false;
                    jump = 1;
                }
            } else {
                jump++;
                if (value >= jump) {
                    result = true;
                }
            }
        }

        return result;
    }
};

int main() {
    Solution solution;
    
    vector<int> nums = {2,3,1,1,4};

    int result = solution.canJump(nums);

    cout << result << endl;
    return 0;
}