#include <iostream>
#include <vector>

using namespace std;

class Solution {
public:
    void merge(vector<int>& nums1, int m, vector<int>& nums2, int n) {
        int nums1Counter = 0;
        int nums2Counter = 0;
        if (m == 0) {
            nums1 = nums2;
            return;
        }

        vector<int> nums1Bis(m);

        for (int i=0; i<m; i++) {
            nums1Bis[i] = nums1[i];
        }

        for (int i=0; i<m+n; i++) {
            if (nums1Counter >= m) {
                nums1[i] = nums2[nums2Counter];
                nums2Counter++;
                continue;
            }
            if (nums2Counter >= n) {
                nums1[i] = nums1Bis[nums1Counter];
                nums1Counter++;
                continue;
            }

            const int a = nums1Bis[nums1Counter];
            const int b = nums2[nums2Counter];
            if (a < b) {
                nums1[i] = a;
                nums1Counter++;
            } else {
                nums1[i] = b;
                nums2Counter++;
            }
        }
    }
};

int main() {
    Solution solution;
    
    vector<int> nums1 = {1,2,3,0,0,0};
    int m = 3;
    vector<int> nums2 = {2,5,6};
    int n = 3;
    
    solution.merge(nums1, m, nums2, n);

    for (int elem : nums1) {
        cout << elem << " ";
    }
    cout << endl; // Print a newline at the end
    return 0;
}