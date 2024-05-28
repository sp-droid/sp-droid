#include <iostream>
#include <unordered_map>
#include <vector>

using namespace std;

class Solution {
public:
    void rotate(vector<int>& nums, int k) {
        // Translations of size n produce the same array, so we can reduce them always to some number below n
        const int n = nums.size();
        k = k % n;
        
        vector<int> destination(k, 0);
        copy(nums.begin() + n-k, nums.begin() + n, destination.begin() + 0);

        copy(nums.begin() + 0, nums.begin() + n-k, nums.begin() + k);
        copy(destination.begin() + 0, destination.begin() + k, nums.begin() + 0);
    }
};

int main() {
    Solution solution;
    
    vector<int> nums = {1,2,3,4,5,6,7};
    int k = 3;

    solution.rotate(nums, k);

    for (int elem : nums) {
        cout << elem << " ";
    }
    cout << endl;
    return 0;
}