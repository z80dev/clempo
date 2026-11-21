(in-package #:clempo.tip20)

;;; RolesAuth (crates/precompiles/src/tip20/roles.rs): each role has an admin role;
;;; DEFAULT_ADMIN_ROLE (zero) is the root, UNGRANTABLE_ROLE administers itself.

(defevent role-membership-updated
  "RoleMembershipUpdated(bytes32 indexed role, address indexed account, address indexed sender, bool hasRole)")
(defevent role-admin-updated
  "RoleAdminUpdated(bytes32 indexed role, bytes32 indexed newAdminRole, address indexed sender)")

(defun has-role (token account role) (read (token-roles token account role)))
(defun get-role-admin (token role) (read (token-role-admins token role)))
(defun grant-role-internal (token account role) (write (token-roles token account role) t))

(defun check-role (token account role)
  (unless (has-role token account role) (revert 'unauthorized)))

(defun initialize-roles (token)
  (write (token-role-admins token +ungrantable-role+) +ungrantable-role+))

(defun grant-default-admin (token sender admin)
  (grant-role-internal token admin +default-admin-role+)
  (emit token 'role-membership-updated +default-admin-role+ admin sender t))

(defun grant-role (token sender role account)
  "IRolesAuth::grantRole by SENDER."
  (check-role token sender (get-role-admin token role))
  (grant-role-internal token account role)
  (emit token 'role-membership-updated role account sender t)
  (values))

(defun revoke-role (token sender role account)
  (check-role token sender (get-role-admin token role))
  (write (token-roles token account role) nil)
  (emit token 'role-membership-updated role account sender nil)
  (values))

(defun renounce-role (token sender role)
  (check-role token sender role)
  (write (token-roles token sender role) nil)
  (emit token 'role-membership-updated role sender sender nil)
  (values))

(defun set-role-admin (token sender role admin-role)
  (check-role token sender (get-role-admin token role))
  (write (token-role-admins token role) admin-role)
  (emit token 'role-admin-updated role admin-role sender)
  (values))

(defcall tip20-token has-role* ((account address) (role b256))
    (:sol "hasRole(address account, bytes32 role)" :returns "bool")
  (has-role self account role))
(defcall tip20-token get-role-admin* ((role b256))
    (:sol "getRoleAdmin(bytes32 role)" :returns "bytes32")
  (get-role-admin self role))
(defcall tip20-token grant-role* ((role b256) (account address))
    (:sol "grantRole(bytes32 role, address account)" :mutates t)
  (grant-role self sender role account))
(defcall tip20-token revoke-role* ((role b256) (account address))
    (:sol "revokeRole(bytes32 role, address account)" :mutates t)
  (revoke-role self sender role account))
(defcall tip20-token renounce-role* ((role b256))
    (:sol "renounceRole(bytes32 role)" :mutates t)
  (renounce-role self sender role))
(defcall tip20-token set-role-admin* ((role b256) (admin-role b256))
    (:sol "setRoleAdmin(bytes32 role, bytes32 adminRole)" :mutates t)
  (set-role-admin self sender role admin-role))
