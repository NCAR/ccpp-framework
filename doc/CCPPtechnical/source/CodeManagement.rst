..  _CodeManagement:

**************************************************
CCPP Code Management
**************************************************

================================
Organization of the Code
================================

This chapter describes the organization of the code, provides instruction on the GitHub workflow and the code review process, and outlines the release procedure. It is assumed that the reader is familiar with using basic GitHub features and has a GitHub account set up.

-----------------------
UFS Atmosphere
-----------------------

The :term:`UFS` Atmosphere source code is contained in the NEMSfv3gfs code repository. CCPP users and developers should use the NEMSfv3gfs code and submodules maintained by GMTB in GitHub. For codes whose authoritative repository is in VLab, GMTB synchronizes these with VLab at periodic intervals. 

https://github.com/NCAR/NEMSfv3gfs

There are five submodules referenced in the NEMSfv3gfs repository

https://github.com/NCAR/ccpp-framework

https://github.com/NCAR/ccpp-physics

https://github.com/NCAR/FV3

https://github.com/NCAR/NEMS

https://github.com/NCAR/FMS

Users have read-only access to these repositories and as such cannot accidentally destroy any important (shared) branches of these authoritative repositories.

Some of these repositories are public (no GitHub account required) and some are private. The public repositories (ccpp-framework, ccpp-physics, and Flexible Modeling System - FMS) may be used directly to read or create forks. Write permission is generally restricted, however. The private repositories require access - please send a request and your GitHub username to gmtb-help@ucar.edu.

The primary development by GMTB, including the latest CCPP developments, are maintained in the following branches:


+---------------------------------------------+----------------------+
| Repository (GMTB development version)       |    Branch name       |
+=============================================+======================+
| https://github.com/NCAR/NEMSfv3gfs          |    gmtb/ccpp         |
+---------------------------------------------+----------------------+
| https://github.com/NCAR/FV3                 |    gmtb/ccpp         |
+---------------------------------------------+----------------------+
| https://github.com/NCAR/ccpp-physics        |   master             |
+---------------------------------------------+----------------------+
| https://github.com/NCAR/ccpp-framework      |   master             |
+---------------------------------------------+----------------------+
| https://github.com/NCAR/NEMS                |   gmtb/ccpp          |
+---------------------------------------------+----------------------+
| https://github.com/NCAR/FMS                 |    GFS-FMS           |
+---------------------------------------------+----------------------+

-----------------------
SCM
-----------------------

CCPP developers should use the SCM code and submodules maintained by GMTB in GitHub. The ccpp-framework and ccpp-physics repositories are used by the two models, creating a two-way dependency between them (see :ref:`committing-changes` for implications on the code review and testing procedure).

https://github.com/NCAR/gmtb-scm
 
As for NEMSfv3gfs, there are two submodules referenced in the gmtb-scm repository:

https://github.com/NCAR/ccpp-framework

https://github.com/NCAR/ccpp-physics

Users have read-only access to these repositories and as such cannot accidentally destroy any important (shared) branches of these authoritative repositories. Both CCPP repositories are public (no GitHub account required) and may be used directly to read or create forks. Write permission is generally restricted, however. The SCM repository is private, to request access please send a message and your GitHub username to gmtb-help@ucar.edu.

The primary development by GMTB, including the latest CCPP developments, are maintained in the following branches:

+----------------------------------------+-------------------+
| Repository (GMTB development version)  | Branch name       |
+========================================+===================+
| https://github.com/NCAR/gmtb-scm       | master            |
+----------------------------------------+-------------------+
| https://github.com/NCAR/ccpp-physics   |master             |
+----------------------------------------+-------------------+
| https://github.com/NCAR/ccpp-framework |master             |
+----------------------------------------+-------------------+

--------------------------------------
Directory Structure of ccpp/framework
--------------------------------------

The following is the directory structure for the ccpp/framework (condensed version):

.. code-block:: console

   ├── cmake                  # cmake files for building
   ├── doc                    # Documentation for design/implementation
   │                          # and developers guide
   │   ├── CCPPtechnical      # CCPP Technical documentation
   │   │   ├── build
   │   │   └── source
   │   │       ├── _static
   │   │       └── _templates
   │   ├── common            # Latex style file used by various documentation efforts
   │   ├── DevelopersGuide
   │   │   └── images
   │   └── img
   ├── schemes                # Example ccpp_prebuild_config.py
   │   ├── check
   ├── scripts                # Scripts for ccpp_prebuild.py, metadata
   │                          # parser, etc.
   │   ├── fortran_tools
   │   └── parse_tools
   ├── src                    # CCPP framework source code
   │   └── tests              # SDFs and code for testing
   ├── test
   │   └── nemsfv3gfs         # NEMSfv3gfs regression test scripts
   └── tests                  # Development for framework upgrades


--------------------------------------
Directory Structure of ccpp/physics
--------------------------------------

The following is the directory structure for the ccpp/physics (condensed version):

.. code-block:: console

   ├── physics                 # CCPP physics source code
   │   ├── docs                # Scientific documentation (doxygen)
   │   │   ├── img             # Figures for doxygen
   │   │   └── pdftxt          # Text files for documentation
   └── stochastic_physics      # Source code for stochastic physics



=====================================================
GitHub Workflow (setting up development repositories)
=====================================================

The CCPP development practices make use of the GitHub forking workflow. For users not familiar with this concept, this website provides some background information and a tutorial.

---------------
Creating Forks
---------------

The GitHub forking workflow relies on forks (personal copies) of the shared repositories on GitHub. These forks need to be created only once, and only for directories that users will contribute changes to. The following steps describe how to create a fork for the example of the ccpp-physics submodule/repository:

 Go to https://github.com/NCAR/ccpp-physics and make sure you are signed in as your GitHub user.

 Select the "fork" button in the upper right corner.
      * If you have already created a fork, this will take you to your fork.
      * If you have not yet created a fork, this will create one for you.

 Note that the repo name in the upper left (blue) will be either "NCAR" or "your GitHub name” which tells you which fork you are looking at.

Note that personal forks are not required until a user wishes to make code contributions. The procedure for how to check out the code laid out below can be followed without having created any forks beforehand.

-----------------------------------
Checking out the Code (NEMSfv3gfs)
-----------------------------------
Because of the large number of submodules of the central repository NEMSfv3gfs, we strongly recommend setting up passwordless access to GitHub via https or ssh (see https://help.github.com/categories/authenticating-to-github). In the following, we assume access is via https rather than ssh.

Start with checking out the main repository from the NCAR GitHub

.. code-block:: console

   git clone -b gmtb/ccpp https://github.com/NCAR/NEMSfv3gfs
   cd NEMSfv3gfs
   git submodule init
   git submodule update

At this point, you have checked out the correct branches of all six repositories. Each repository is connected to a remote destination (simply called remote in GitHub language), which points to the NCAR GitHub pages and is labeled as origin. For consistency with the CCPP workflow, we recommend renaming the NCAR remote destination to upstream for all six repositories in order to guarantee that your development will be pushed to your fork and not directly to the NCAR repository. For example:

.. code-block:: console

   cd ccpp/framework
   git remote rename origin upstream
   cd ../..

Checking out remote branches as submodules means that your local branches are in a detached head state, since the submodule points to a specific commit. As long as you are not making any modifications in a certain repository, this is not a problem. If during your development changes are made to the corresponding upstream branch, you can simply navigate to this repository and check out the updated version (example NEMS):

.. code-block:: console

   cd NEMS
   git remote update
   git checkout upstream/gmtb/ccpp
   cd ..

However, if you are making changes in a repository (submodule or main repository), you must create a local branch, for example in NEMSfv3gfs:

.. code-block:: console

   git checkout -b my_local_development_branch
 
Once you are ready to contribute the code to the upstream repository, you need to create a pull request (PR; see next section). In order to do so, you will use your own fork of this repository (see previous section) and configure your fork as an additional remote destination, which we typically label as origin. For the same example NEMSfv3gfs:

.. code-block:: console

   git remote add origin https://github.com/YOUR_GITHUB_USER/NEMSfv3gfs
   git remote update

Then, push your local branch to your fork:

.. code-block:: console

   git push origin my_local_development_branch

For each repository/submodule, you can check the configured remote destinations and all existing branches (remote and local):

.. code-block:: console

   git remote -v show
   git remote update
   git branch -a

As opposed to branches without modifications described in step 3, changes to the upstream repository can be brought into the local branch by pulling them down. For the example of submodule FV3 (where a local branch is checked out):

.. code-block:: console

   cd FV3
   git remote update
   git pull upstream gmtb/ccpp


-----------------------------------
Checking out the Code (SCM)
-----------------------------------
The process for checking out SCM is described in the following, assuming access via https rather than ssh. We strongly recommend setting up passwordless access to GitHub (see https://help.github.com/categories/authenticating-to-github).

Start with checking out the main repository from the NCAR GitHub

.. code-block:: console

   git clone https://github.com/NCAR/gmtb-scm
   cd gmtb-scm
   git submodule init
   git submodule update

At this point, you have checked out the correct branches of all three repositories. Each repository is connected to a remote destination (simply called remote in GitHub language), which points to the NCAR GitHub pages and is labeled as origin. For consistency with the CCPP workflow, we recommend renaming the NCAR remote destination to upstream for all repositories. For example:

.. code-block:: console

   cd ccpp/framework
   git remote rename origin upstream
   cd ../..

Checking out remote branches means that your local branches are in a detached state, since you cannot commit directly to a remote branch. As long as you are not making any modifications in a certain repository, this is not a problem. If during your development work changes are made to the corresponding upstream branch, you can simply navigate to this repository and check out the updated version (example ccpp-physics):

.. code-block:: console

   cd ccpp/physics
   git remote update
   git checkout upstream/master
   cd ../..

However, if you are making changes in a repository (submodule or main repository), you must create a local branch, for example in gmtb-scm:

.. code-block:: console

   git checkout -b my_local_development_branch
 
Once you are ready to contribute the code to the upstream repository, you need to create a PR (see next section). In order to do so, you first need to create your own fork of this repository (see previous section) and configure your fork as an additional remote destination, which we typically label as origin. For the same example gmtb-scm:

.. code-block:: console
 
   git remote add origin https://github.com/YOUR_GITHUB_USER/gmtb-scm
   git remote update

Then, push your local branch to your fork:

.. code-block:: console

   git push origin my_local_development_branch

For each repository/submodule, you can check the configured remote destinations and all existing branches (remote and local):

.. code-block:: console

   git remote -v show
   git remote update
   git branch -a
 
As opposed to branches without modifications described in step 3, changes to the upstream repository can be brought into the local branch by pulling them down. For the example of submodule ccpp-physics (where a local branch is checked out):

.. code-block:: console

   cd ccpp/physics
   git remote update
   git pull upstream master

.. _committing-changes:

==================================
Committing Changes to your Fork
==================================
Once you have your fork set up to begin code modifications, you should check that the cloned repositories upstream and origin are set correctly:
		
.. code-block:: console

   git remote -v

This should point to your fork as origin and the repository you cloned as upstream:

.. code-block:: console

   origin	      https://github.com/YOUR_GITHUB_USER/ccpp-physics (fetch)
   origin	      https://github.com/YOUR_GIRHUB_USER/ccpp-physics (push)
   upstream   https://github.com/NCAR/ccpp-physics (fetch)
   upstream   https://github.com/NCAR/ccpp-physics (push)

Also check what branch you are working on:

.. code-block:: console

   git branch

This command will show what branch you have checked out on your fork:

.. code-block:: console

   * features/my_local_development_branch
     master

After making modifications and testing, you can commit the changes to your fork.  First check what files have been modified:

.. code-block:: console

   git status

This git command will provide some guidance on what files need to be added and what files are “untracked”.  To add new files or stage modified files to be committed:

.. code-block:: console

   git add filename1 filename2

At this point it is helpful to have a description of your changes to these files documented somewhere, since when you commit the changes, you will be prompted for this information.  To commit these changes to your local repository and push them to the development branch on your fork:

.. code-block:: console

   git commit
   git push origin features/my_local_development_branch

When this is done, you can check the status again:

.. code-block:: console

   git status

This should show that your working copy is up to date with what is in the repository:

.. code-block:: console

   On branch features/my_local_development_branch
   Your branch is up to date with 'origin/features/my_local_development_branch'.
   nothing to commit, working tree clean

At this point you can continue development or create a PR as discussed in the next section.

=========================================
Contributing Code, Code Review Process
=========================================
Once your development is mature, and the testing has been completed (see next section), you are ready to create a PR using GitHub to propose your changes for review.

-------------------
Creating a PR
-------------------
Go to the github.com web interface, and navigate to your repository fork and branch. In most cases, this will be in the ccpp-physics repository, hence the following example:

 | Navigate to: https://github.com/<yourusername>/ccpp-physics
 | Use the drop-down menu on the left-side to select a branch to view your development branch
 | Use the button just right of the branch menu, to start a “New Pull Request”
 | Fill in a short title (one line)
 | Fill in a detailed description, including reporting on any testing you did
 | Click on “Create pull request”

Several people (aka CODEOWNERS) are automatically added to the list of reviewers on the right hand side. If others should be reviewing the code, click on the “reviewers” item on the right hand side and enter their GitHub usernames

Once the PR has been approved, the change is merged to master by one of the code owners. If there are pending conflicts, this means that the code is not up to date with the trunk. To resolve those, pull the target branch from upstream as described above, solve the conflicts and push the changes to the branch on your fork (this also updates the PR).

Note. GitHub offers a draft pull request feature that allows users to push their code to GitHub and create a draft PR. Draft PRs cannot be merged and do not automatically initiate notifications to the CODEOWNERS, but allow users to prepare the PR and flag it as “ready for review” once they feel comfortable with it.

